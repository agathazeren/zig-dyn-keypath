const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const testing = std.testing;
const builtin = std.builtin;
const debug = std.debug;

const recursive = @import("recursive.zig");
const RecursiveField = recursive.RecursiveField;

pub fn Keypath(T: type) type {}

pub fn DynFieldType(T: type) type {}

pub fn RecursiveKeypath(T: type) type {}

pub fn DynRecFieldType(T: type) type {}
