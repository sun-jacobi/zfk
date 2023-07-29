const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const Error = error{ OutofMemory, InvalidInstr, InvalidProgram, PtrOutScope };

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
            else => Error.InvalidInstr,
        };
    }
};

const Machine = struct {
    tape: std.ArrayList(u8),

    ptr: usize, // address pointer (current cell)
    pc: usize, // program counter (current instruction)

    fn init(self: *Machine, allocator: std.mem.Allocator) void {
        self.tape = std.ArrayList(u8).init(allocator);
        self.ptr = 0;
        self.pc = 0;
    }

    fn deinit(self: *Machine) void {
        self.tape.deinit();
        self.ptr = 0;
        self.pc = 0;
    }

    // collect the loop match
    fn scan(src: []const u8, allocator: std.mem.Allocator) Error!std.AutoHashMap(usize, usize) {
        var stack = std.ArrayList(usize).init(allocator);
        var map = std.AutoHashMap(usize, usize).init(allocator);
        for (src, 0..) |c, i| {
            if (c == '[') {
                stack.append(i) catch {
                    return Error.OutofMemory;
                };
            }
            if (c == ']') {
                if (stack.popOrNull()) |j| {
                    map.put(j, i) catch {
                        return Error.OutofMemory;
                    };
                } else {
                    return Error.InvalidProgram;
                }
            }
        }
        return map;
    }

    // run a single line of code
    fn run(self: *Machine, src: []const u8, allocator: std.mem.Allocator) !void {
        self.init(allocator);
        defer self.deinit();
        const len = src.len;
        const map = try scan(src, allocator);

        while (self.pc < len) {
            const inst = try Instruction.translate(src[self.pc]);
            self.pc += 1;
            try self.exec(inst, &map);
        }
    }

    fn exec(self: *Machine, inst: Instruction, map: *const std.AutoHashMap(usize, usize)) !void {
        _ = map;

        if (self.ptr >= self.tape.items.len) {
            try self.tape.append(0);
        }

        switch (inst) {
            .left => {
                const res = @subWithOverflow(self.ptr, 1);
                if (res[1] == 0) {
                    self.ptr = res[0];
                } else {
                    return Error.PtrOutScope;
                }
            },
            .right => {
                const res = @addWithOverflow(self.ptr, 1);
                if (res[1] == 0) {
                    self.ptr = res[0];
                } else {
                    return Error.PtrOutScope;
                }
            },
            .incr => {
                const prev = self.tape.items[self.ptr];
                self.tape.items[self.ptr] = @addWithOverflow(prev, 1)[0];
            },
            .decr => {
                const prev = self.tape.items[self.ptr];
                self.tape.items[self.ptr] = @subWithOverflow(prev, 1)[0];
            },
            .write => {
                const value = self.tape.items[self.ptr];
                try stdout.print("{c}", .{value});
            },
            else => {
                return Error.InvalidInstr;
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
        .tape = undefined,
        .ptr = undefined,
        .pc = undefined,
    };
    defer machine.deinit();

    //=================================================
    try stdout.print("Welcome to zfk\n", .{});
    while (true) {
        try stdout.print("zfk:>> ", .{});
        var buf: [100]u8 = undefined;

        if (try stdin.readUntilDelimiterOrEof(buf[0..], '\n')) |code| {
            machine.init(gpa);
            try machine.run(code, gpa);
            try stdout.print("\n", .{});
        }
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
