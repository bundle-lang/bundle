const std = @import("std");
const fs = std.fs;
const log = std.log;
const process = std.process;

const lex = @import("lex.zig");
const parse = @import("parse.zig");
const name_resolve = @import("name_resolve.zig");
const type_check = @import("type_check.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var arena = std.heap.ArenaAllocator.init(gpa.allocator());
const allocator = arena.allocator();

pub fn main() !void {
    defer _ = gpa.deinit();
    defer arena.deinit();

    const args = try process.argsAlloc(allocator);

    if (args.len == 2) {
        const file = args[1];
        const src = fs.cwd().readFileAlloc(allocator, file, 1024 * 1024 * 64) catch |err| {
            log.err("file reading failed with {}", .{err});
            process.exit(1);
        };

        var lexer = lex.new(file, src);
        var parser = parse.new(allocator, lexer);

        parser.parse() catch |err| {
            log.debug("parsing failed, got {}", .{err});
            process.exit(1);
        };

        const decl_table = name_resolve.new(allocator, parser.nodes).build(); // TODO: proper error reporting.
        type_check.new(parser.nodes, decl_table).check();
    } else {
        log.err("specify a file name", .{});
    }
}
