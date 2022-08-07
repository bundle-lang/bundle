const std = @import("std");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;

const ScopeChildren = std.ArrayList(*Scope);
const Declarations = std.StringHashMap(ast.NodeKind);

pub const Scope = struct {
    outer: ?*Scope,
    children: ScopeChildren,
    declarations: Declarations,

    pub fn declare(self: *Scope, name: []const u8, definition: ast.NodeKind) void {
        if (self.contains(name)) {
            std.log.err("redeclaration of `{s}`", .{name});
        }

        self.declarations.put(name, definition) catch unreachable;
    }

    pub fn findDecl(self: *Scope, name: []const u8) ?ast.NodeKind {
        var scope: ?*Scope = self;

        while (scope) |s| : (scope = s.outer) {
            if (s.declarations.get(name)) |decl| {
                return decl;
            }
        }

        return null;
    }

    pub fn contains(self: *Scope, name: []const u8) bool {
        var scope: ?*Scope = self;

        while (scope) |s| : (scope = s.outer) {
            if (s.declarations.contains(name)) {
                return true;
            }
        }

        return false;
    }
};

pub fn new(allocator: Allocator, outer: ?*Scope) *Scope {
    const scope = allocator.create(Scope) catch unreachable;

    scope.* = .{
        .outer = outer,
        .children = ScopeChildren.init(allocator),
        .declarations = Declarations.init(allocator),
    };

    if (outer) |s| {
        s.children.append(scope) catch unreachable;
    }

    return scope;
}