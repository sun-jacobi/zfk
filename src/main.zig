const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();
const assert = std.debug.assert;

const Error = error{ OutofMemory, InvalidInstr, InvalidProgram, PtrOutScope, EOF };

const Instruction = enum {
    left, // move the pointer left
    right, // move the pointer right
    incr, // increment the current cell
    decr, // decrement the current cell
    write, // output the value of the current cell
    read, // replace the value of the current cell with input
    begin,
    end,
    fn translate(instr: u8) !Instruction {
        return switch (instr) {
            '<' => .left,
            '>' => .right,
            '+' => .incr,
            '-' => .decr,
            '.' => .write,
            ',' => .read,
            '[' => .begin,
            ']' => .end,
            else => {
                return Error.InvalidInstr;
            },
        };
    }
};

const Machine = struct {
    left_tape: std.ArrayList(u8),
    right_tape: std.ArrayList(u8),

    negative: bool, // use or not left tape
    left_ptr: usize, // address pointer for left tape
    right_ptr: usize, // address pointer for right tape
    pc: usize, // program counter (current instruction)

    cursor: usize, // input pointer

    const LoopMap = struct {
        left2right: std.AutoHashMap(usize, usize),
        right2left: std.AutoHashMap(usize, usize),
        fn deinit(self: *LoopMap) void {
            self.left2right.deinit();
            self.right2left.deinit();
        }
    };

    fn init(self: *Machine, allocator: std.mem.Allocator) void {
        self.left_tape = std.ArrayList(u8).init(allocator);
        self.right_tape = std.ArrayList(u8).init(allocator);
        self.negative = false;
        self.left_ptr = 1;
        self.right_ptr = 0;
        self.pc = 0;
        self.cursor = 0;
    }

    fn deinit(self: *Machine) void {
        self.left_tape.deinit();
        self.right_tape.deinit();
        self.left_ptr = undefined;
        self.right_ptr = undefined;
        self.negative = false;
        self.pc = undefined;
        self.cursor = undefined;
    }

    // collect the loop match
    fn scan(src: []const u8, allocator: std.mem.Allocator) Error!LoopMap {
        var stack = std.ArrayList(usize).init(allocator);
        defer stack.deinit();
        var left2right = std.AutoHashMap(usize, usize).init(allocator);
        var right2left = std.AutoHashMap(usize, usize).init(allocator);
        for (src, 0..) |c, i| {
            if (c == '[') {
                stack.append(i) catch {
                    return Error.OutofMemory;
                };
            }
            if (c == ']') {
                const right = i;
                if (stack.popOrNull()) |left| {
                    left2right.put(left, right) catch {
                        return Error.OutofMemory;
                    };
                    right2left.put(right, left) catch {
                        return Error.OutofMemory;
                    };
                } else {
                    return Error.InvalidProgram;
                }
            }
        }
        return LoopMap{ .left2right = left2right, .right2left = right2left };
    }

    // run a single line of code
    fn run(self: *Machine, src: []const u8, input: []const u8, allocator: std.mem.Allocator) !void {
        self.init(allocator);
        defer self.deinit();
        const len = src.len;
        var map = try scan(src, allocator);
        defer map.deinit();

        while (self.pc < len) {
            const inst = try Instruction.translate(src[self.pc]);

            try self.exec(inst, &map, input);
            self.pc += 1;
        }
    }

    fn read_tape(self: Machine) u8 {
        if (self.negative) {
            return self.left_tape.items[self.left_ptr - 1];
        } else {
            return self.right_tape.items[self.right_ptr];
        }
    }

    fn write_tape(self: *Machine, val: u8) void {
        if (self.negative) {
            self.left_tape.items[self.left_ptr - 1] = val;
        } else {
            self.right_tape.items[self.right_ptr] = val;
        }
    }

    fn move_right(self: *Machine) !void {
        if (self.negative) {
            if (self.left_ptr == 1) {
                self.negative = false; // start using right tape
                assert(self.right_ptr == 0);
            } else {
                self.left_ptr -= 1;
            }
        } else {
            const res = @addWithOverflow(self.right_ptr, 1);
            if (res[1] == 0) {
                self.right_ptr = res[0];
            } else {
                return Error.PtrOutScope;
            }
        }
    }

    fn move_left(self: *Machine) !void {
        if (!self.negative) {
            if (self.right_ptr == 0) {
                self.negative = true; // start using left tape
                assert(self.left_ptr == 1);
            } else {
                self.right_ptr -= 1;
            }
        } else {
            const res = @addWithOverflow(self.left_ptr, 1);
            if (res[1] == 0) {
                self.left_ptr = res[0];
            } else {
                return Error.PtrOutScope;
            }
        }
    }

    fn exec(self: *Machine, inst: Instruction, map: *const LoopMap, input: []const u8) !void {
        if (self.negative and self.left_ptr - 1 >= self.left_tape.items.len) {
            try self.left_tape.append(0);
        } else if (self.right_ptr >= self.right_tape.items.len) {
            try self.right_tape.append(0);
        }

        switch (inst) {
            .left => {
                return self.move_left();
            },
            .right => {
                return self.move_right();
            },
            .incr => {
                const prev = self.read_tape();
                self.write_tape(@addWithOverflow(prev, 1)[0]);
            },
            .decr => {
                const prev = self.read_tape();
                self.write_tape(@subWithOverflow(prev, 1)[0]);
            },
            .write => {
                const value = self.read_tape();
                try stdout.print("{c}", .{value});
            },
            .read => {
                if (self.cursor >= input.len) {
                    return Error.EOF;
                }
                const value = input[self.cursor];
                self.cursor += 1;

                self.write_tape(value);
            },

            .begin => {
                const value = self.read_tape();
                if (value != 0) return; // do nothing
                if (map.left2right.get(self.pc)) |next| {
                    self.pc = next;
                } else {
                    return Error.InvalidProgram;
                }
            },

            .end => {
                const value = self.read_tape();
                if (value == 0) return; // do nothing
                if (map.right2left.get(self.pc)) |next| {
                    self.pc = next;
                } else {
                    return Error.InvalidProgram;
                }
            },
        }
    }
};

pub fn main() !void {
    // setup the memory allocator
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
    const gpa = general_purpose_allocator.allocator();

    // setup the brainfuck machine
    var machine = Machine{
        .left_tape = undefined,
        .right_tape = undefined,
        .negative = undefined,
        .left_ptr = undefined,
        .right_ptr = undefined,
        .pc = undefined,
        .cursor = undefined,
    };

    //=================================================
    try stdout.print("Welcome to zfk\n", .{});
    while (true) {
        try stdout.print("program:>> ", .{});
        var pbuf: [1000]u8 = undefined;
        var ibuf: [1000]u8 = undefined;

        if (try stdin.readUntilDelimiterOrEof(pbuf[0..], '\n')) |code| {
            try stdout.print("input:>> ", .{});
            if (try stdin.readUntilDelimiterOrEof(ibuf[0..], '\n')) |input| {
                if (machine.run(code, input, gpa)) {} else |err| switch (err) {
                    Error.EOF => {},
                    else => {
                        try stdout.print("{}", .{err});
                    },
                }
                try stdout.print("\n", .{});
            }
        }
    }
}

test "simple test" {}
