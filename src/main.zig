const std = @import("std");
const fs = std.fs;
const log = std.log;
const process = std.process;

const lex = @import("lex.zig");
const parse = @import("parse.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var arena = std.heap.ArenaAllocator.init(gpa.allocator());
const allocator = arena.allocator();

pub fn main() !void {
    defer _ = gpa.deinit();
    defer arena.deinit();

    const args = try process.argsAlloc(allocator);

    if (args.len == 2) {
        const file = args[1];
        const src = fs.cwd().readFileAlloc(allocator, file, 1024 * 1024 * 64) catch {
            log.err("the file to be read is too large", .{});
            return;
        };

        var lexer = lex.new(file, src);
        var parser = parse.new(allocator, lexer);

        parser.parse() catch |err| {
            log.debug("parsing failed, got {}", .{err});
            return;
        };
    } else {
        log.err("specify a file name", .{});
    }
}
