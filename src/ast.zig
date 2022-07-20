const std = @import("std");
const lex = @import("lex.zig");

pub const NodeArray = std.ArrayList(NodeKind);

pub const NodeKind = union(enum) {
    fn_decl: NodeFnDecl,
    var_decl: NodeVarDecl,
    if_stmt: NodeIfStmt,
    elif_stmt: NodeElifStmt,
    return_stmt: NodeReturnStmt,
    arg: NodeArg,
    basic_expr: NodeBasicExpr,
    binary_expr: NodeBinaryExpr,
};

pub const Type = enum {
    type_u32,
    type_i32,
    type_bool,
};

pub const Operator = enum {
    plus,
    minus,
    star,
    slash,
};

pub const NodeBasicExpr = union(enum) {
    identifier: []const u8,
    integer: u32,
    boolean: bool,
};

pub const NodeFnDecl = struct {
    name: []const u8,
    args: NodeArray,
    fn_type: Type,
    body: NodeArray,
};

pub const NodeVarDecl = struct {
    name: []const u8,
    let_type: Type,
    value: *NodeKind,
};

pub const NodeIfStmt = struct {
    if_condition: *NodeKind,
    if_body: NodeArray,
    elif_nodes: ?NodeArray,
    else_body: ?NodeArray,
};

pub const NodeElifStmt = struct {
    elif_condition: *NodeKind,
    elif_body: NodeArray,
};

pub const NodeReturnStmt = struct {
    value: *NodeKind,
};

pub const NodeArg = struct {
    name: []const u8,
    arg_type: Type,
};

pub const NodeBinaryExpr = struct {
    left: *NodeKind,
    operator: Operator,
    right: *NodeKind,
};
