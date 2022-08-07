const std = @import("std");

pub const NodeArray = std.ArrayList(NodeKind);
pub const NodeId = u32;

pub const NodeKind = union(enum) {
    block_stmt: NodeBlockStmt,
    fn_decl: NodeFnDecl,
    let_stmt: NodeLetStmt,
    assign_stmt: NodeAssignStmt,
    if_stmt: NodeIfStmt,
    elif_stmt: NodeElifStmt,
    return_stmt: NodeReturnStmt,
    arg: NodeArg,
    reference: NodeReference,
    grouping_expr: NodeGroupingExpr,
    literal_expr: NodeLiteralExpr,
    unary_expr: NodeUnaryExpr,
    binary_expr: NodeBinaryExpr,
    call_expr: NodeCallExpr,
};

pub const Type = enum {
    type_i32,
    type_bool,
};

pub const Operator = enum {
    plus,
    minus,
    star,
    slash,
};

pub const NodeBlockStmt = struct {
    list: NodeArray,
};

pub const NodeFnDecl = struct {
    name: []const u8,
    args: NodeArray,
    fn_type: Type,
    body: NodeBlockStmt,
};

pub const NodeLetStmt = struct {
    name: []const u8,
    let_type: Type,
    value: *NodeKind,
};

pub const NodeAssignStmt = struct {
    left_expr: *NodeKind,
    value: *NodeKind,
};

pub const NodeIfStmt = struct {
    if_condition: *NodeKind,
    if_body: NodeBlockStmt,
    elif_nodes: ?NodeArray,
    else_body: ?NodeBlockStmt,
};

pub const NodeElifStmt = struct {
    elif_condition: *NodeKind,
    elif_body: NodeBlockStmt,
};

pub const NodeReturnStmt = struct {
    value: *NodeKind,
};

pub const NodeArg = struct {
    name: []const u8,
    arg_type: Type,
};

pub const NodeReference = struct {
    id: NodeId,
    name: []const u8,
};

pub const NodeGroupingExpr = struct {
    expr: *NodeKind,
};

pub const NodeLiteralExpr = union(enum) {
    integer: u32,
    boolean: bool,
};

pub const NodeUnaryExpr = struct {
    operator: Operator,
    expr: *NodeKind,
};

pub const NodeBinaryExpr = struct {
    left: *NodeKind,
    operator: Operator,
    right: *NodeKind,
};

pub const NodeCallExpr = struct {
    left_expr: *NodeKind,
    args: NodeArray,
};
