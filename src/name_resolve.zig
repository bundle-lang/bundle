const std = @import("std");
const log = std.log;

const ast = @import("ast.zig");
const visitor = @import("visitor.zig");
const scope = @import("scope.zig");

const Allocator = std.mem.Allocator;
pub const DeclTable = std.AutoHashMap(ast.NodeId, ast.NodeKind);

const GlobalNameDecl = struct {
    nodes: ast.NodeArray,
    global_scope: *scope.Scope,

    pub fn visitFnDecl(self: *GlobalNameDecl, node: ast.NodeFnDecl) void {
        self.global_scope.declare(node.name, .{ .fn_decl = node });
    }

    pub fn visitExternDecl(self: *GlobalNameDecl, node: ast.NodeExternDecl) void {
        self.global_scope.declare(node.name, .{ .extern_decl = node });
    }

    fn run(self: *GlobalNameDecl) void {
        const global_name_decl_visitor = visitor.new(self);
        visitor.traverseArray(self.nodes, global_name_decl_visitor);
    }

    fn new(allocator: Allocator, nodes: ast.NodeArray) GlobalNameDecl {
        return GlobalNameDecl{
            .nodes = nodes,
            .global_scope = scope.new(allocator, null),
        };
    }
};

const LocalNameDecl = struct {
    allocator: Allocator,
    nodes: ast.NodeArray,
    global_scope: *scope.Scope,
    scope: *scope.Scope,
    decl_table: DeclTable,

    pub fn visitLetStmt(self: *LocalNameDecl, node: ast.NodeLetStmt) void {
        self.scope.declare(node.name, .{ .let_stmt = node });
    }

    pub fn visitParameter(self: *LocalNameDecl, node: ast.NodeParameter) void {
        self.scope.declare(node.name, .{ .parameter = node });
    }

    pub fn visitReference(self: *LocalNameDecl, node: ast.NodeReference) void {
        if (self.scope.findDecl(node.name)) |decl| {
            self.decl_table.put(node.id, decl) catch unreachable;
        } else if (self.global_scope.findDecl(node.name)) |decl| {
            self.decl_table.put(node.id, decl) catch unreachable;
        } else {
            log.err("undefined reference to `{s}`", .{node.name});
        }
    }

    pub fn enterBlockStmt(self: *LocalNameDecl, _: ast.NodeBlockStmt) void {
        self.scope = scope.new(self.allocator, self.scope);
    }

    pub fn exitBlockStmt(self: *LocalNameDecl, _: ast.NodeBlockStmt) void {
        self.scope = self.scope.outer.?;
    }

    fn run(self: *LocalNameDecl) void {
        const local_name_decl_visitor = visitor.new(self);
        visitor.traverseArray(self.nodes, local_name_decl_visitor);
    }

    fn new(allocator: Allocator, nodes: ast.NodeArray, global_scope: *scope.Scope) LocalNameDecl {
        return LocalNameDecl{
            .allocator = allocator,
            .nodes = nodes,
            .global_scope = global_scope,
            .scope = scope.new(allocator, null),
            .decl_table = DeclTable.init(allocator),
        };
    }
};

const NameResolver = struct {
    allocator: Allocator,
    nodes: ast.NodeArray,

    pub fn build(self: *NameResolver) DeclTable {
        var global_name_decl = GlobalNameDecl.new(self.allocator, self.nodes);
        global_name_decl.run();

        var local_name_decl = LocalNameDecl.new(self.allocator, self.nodes, global_name_decl.global_scope);
        local_name_decl.run();

        return local_name_decl.decl_table;
    }
};

pub fn new(allocator: Allocator, nodes: ast.NodeArray) NameResolver {
    return NameResolver{
        .allocator = allocator,
        .nodes = nodes,
    };
}
