const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const testing = std.testing;
const builtin = std.builtin;
const debug = std.debug;

const TypeInfo = builtin.TypeInfo;

pub const RecursiveField = struct {
    path: []const []const u8,
    field_type: type,
    default_value: anytype,
    is_comptime: bool,
    alignment: comptime_int,

    pub fn of(comptime T: type) []RecursiveField {
        @setEvalBranchQuota(100000);

        var fields: [count(T)]RecursiveField = undefined;
        var i: comptime_int = 0;

        push(RecursiveField, &i, &fields, &[1]RecursiveField{.{
            .path = &[0][]const u8{},
            .field_type = T,
            .default_value = null,
            .is_comptime = false,
            .alignment = @alignOf(T),
        }});

        switch (@typeInfo(T)) {
            .Struct, .Union => for (meta.fields(T)) |field| {
                push(
                    RecursiveField,
                    &i,
                    &fields,
                    addBase(&[1][]const u8{field.name}, RecursiveField.of(field.field_type)),
                );
            },
            else => {},
        }

        // @compileLog(i);
        // for (fields) |field| {
        //     @compileLog(field.path);
        // }

        return &fields;
    }

    fn count(comptime T: type) comptime_int {
        switch (@typeInfo(T)) {
            .Struct, .Union => {
                var n: comptime_int = 1; // the 1 is the field continaing the struct
                for (meta.fields(T)) |field| {
                    n += count(field.field_type);
                }
                return n;
            },
            else => return 1,
        }
    }

    fn addBase(comptime base: []const []const u8, comptime fields: []const RecursiveField) []const RecursiveField {
        var new_fields: [fields.len]RecursiveField = undefined;
        for (fields) |field, i| {
            new_fields[i] = RecursiveField{
                .path = base ++ field.path,
                .field_type = field.field_type,
                .default_value = field.default_value,
                .is_comptime = field.is_comptime,
                .alignment = field.alignment,
            };
        }
        return &new_fields;
    }

    pub fn lessThan(_: void, comptime lhs: RecursiveField, comptime rhs: RecursiveField) bool {
        var l_str = @typeName(lhs.field_type);
        var r_str = @typeName(rhs.field_type);

        // Start with lenth comparison
        if (l_str.len != r_str.len) {
            return l_str.len < r_str.len;
        }

        // fall back to lexical order by bytes.
        var k: usize = 0;
        while (k < l_str.len and l_str[k] == r_str[k]) : (k += 1) {}
        if (k == l_str.len) return false; // equal case

        return l_str[k] < r_str[k];
    }
};

fn push(comptime T: type, comptime i: *comptime_int, array: []T, new: []const T) void {
    mem.copy(T, array[i.*..], new);
    i.* += new.len;
}

test "recursive field on single" {
    const expected = [_]RecursiveField{
        .{ .path = &[_][]const u8{}, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
    };

    const actual = RecursiveField.of(usize);

    inline for (actual) |actual_field, i| {
        const expected_field = expected[i];
        try testing.expectEqual(expected_field.path.len, actual_field.path.len);
        inline for (expected_field.path) |expected_id, j| {
            try testing.expectEqual(expected_field.path.len, actual_field.path.len);
            _ = expected_id;
            _ = j;
        }
    }
}

test "recursive field on struct" {
    const T = struct {
        x: usize,
        y: usize,
    };

    const expected = [_]RecursiveField{
        .{ .path = &[_][]const u8{}, .field_type = T, .default_value = null, .is_comptime = false, .alignment = @alignOf(T) },
        .{ .path = &[_][]const u8{"x"}, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
        .{ .path = &[_][]const u8{"y"}, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
    };

    const actual = RecursiveField.of(T);

    // inline for (actual) |field| {
    //     @compileLog(field.path);
    // }

    try testing.expectEqual(expected.len, actual.len);

    inline for (actual) |actual_field, i| {
        const expected_field = expected[i];
        try testing.expectEqual(expected_field.path.len, actual_field.path.len);
        inline for (expected_field.path) |expected_id, j| {
            try testing.expectEqual(expected_field.path.len, actual_field.path.len);
            _ = expected_id;
            _ = j;
        }
    }
}

test "recursive field on nested" {
    const B = struct {
        j: usize,
    };

    const Z = struct {
        a: usize,
        b: B,
        c: usize,
    };

    const T = struct {
        x: usize,
        y: usize,
        z: Z,
    };

    const expected = [_]RecursiveField{
        .{ .path = &[_][]const u8{}, .field_type = T, .default_value = null, .is_comptime = false, .alignment = @alignOf(T) },
        .{ .path = &[_][]const u8{"x"}, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
        .{ .path = &[_][]const u8{"y"}, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
        .{ .path = &[_][]const u8{"z"}, .field_type = Z, .default_value = null, .is_comptime = false, .alignment = @alignOf(Z) },
        .{ .path = &[_][]const u8{ "z", "a" }, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
        .{ .path = &[_][]const u8{ "z", "b" }, .field_type = B, .default_value = null, .is_comptime = false, .alignment = @alignOf(B) },
        .{ .path = &[_][]const u8{ "z", "b", "j" }, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
        .{ .path = &[_][]const u8{ "z", "c" }, .field_type = usize, .default_value = null, .is_comptime = false, .alignment = @alignOf(usize) },
    };

    const actual = RecursiveField.of(T);

    // inline for (actual) |field| {
    //     @compileLog(field.path);
    // }

    try testing.expectEqual(expected.len, actual.len);

    inline for (actual) |actual_field, i| {
        const expected_field = expected[i];
        try testing.expectEqual(expected_field.path.len, actual_field.path.len);
        inline for (expected_field.path) |expected_id, j| {
            try testing.expectEqual(expected_field.path.len, actual_field.path.len);
            _ = expected_id;
            _ = j;
        }
    }
}
