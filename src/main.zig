const std = @import("std");
const mem = std.mem;
const meta = std.meta;
const testing = std.testing;
const builtin = std.builtin;
const debug = std.debug;
const math = std.math;

const sort = std.sort.sort;

const TypeInfo = builtin.TypeInfo;

const ComptimeStringMap = std.ComptimeStringMap;

const RecursiveField = @import("recursive_field.zig").RecursiveField;
pub const DynRecFieldValue = @import("dyn_rec_field_value.zig").DynRecFieldValue;

//pub fn Keypath(T: type) type {}

//pub fn DynFieldType(T: type) type {}

pub fn RecursiveKeypath(comptime T: type) type {
    const keypath_data = buildKeypathData(T);
    const field_data = keypath_data.field_data;

    var _enum_fields: [field_data.len]TypeInfo.EnumField = undefined;
    for (field_data) |field, i| {
        _enum_fields[i] = TypeInfo.EnumField{
            .name = mangle(field.path),
            .value = field.value,
        };
    }

    comptime var _names: [field_data.len - 1][]const u8 = undefined;
    for (field_data[1..field_data.len]) |field, i| {
        _names[i] = field.path[field.path.len - 1];
    }
    sort([]const u8, &_names, {}, lexicalSort);
    comptime var _name_count = 0;
    for (_names) |name, i| {
        if (i == 0 or !mem.eql(u8, name, _names[i - 1]))
            _name_count += 1;
    }
    comptime var _names_dedup: [_name_count][]const u8 = undefined;
    comptime var _names_j = 0;
    for (_names) |name, i| {
        if (i == 0 or !mem.eql(u8, name, _names[i - 1])) {
            _names_dedup[_names_j] = name;
            _names_j += 1;
        }
    }
    const names = _names_dedup;

    const Inner = @Type(.{ .Enum = TypeInfo.Enum{
        .layout = .Auto,
        .tag_type = keypath_data.stats.Tag(),
        .fields = &_enum_fields,
        .decls = &[0]TypeInfo.Declaration{},
        .is_exhaustive = true,
    } });

    return struct {
        _inner: Inner,

        const Self = @This();

        pub fn root() Self {
            return .{ ._inner = @field(Inner, "__keypath") }; // mangle(&[0][]const u8{}));
        }

        pub fn key(self: Self, name: []const u8) ?Self {
            if (name_id_map.get(name)) |name_id| {
                return subkey_table[self.toInt()][name_id];
            } else {
                return null;
            }
        }

        pub fn up(self: Self) ?Self {
            if (self.eq(root())) {
                return null;
            } else {
                return up_table[self.toInt()];
            }
        }

        pub fn get(self: Self, obj: *const T) DynRecFieldValue(T) {
            if (self.isZst()) {
                return DynRecFieldValue(T).fromZst(self.dynType());
            } else {
                return DynRecFieldValue(T).fromRaw(
                    self.dynType(),
                    @ptrCast([*]const u8, &@ptrCast([*]const u8, obj)[self.byteOffset().?]),
                    @intCast(u3, self.bitOffset().? - self.byteOffset().? * 8),
                );
            }
        }

        //pub fn getDuck(self: Self, Duck: type, obj: Duck) DynRecFieldValue(Duck) {}

        pub fn set(self: Self, obj: *T, value: DynRecFieldValue(T)) void {
            debug.assert(self.dynType() == value.tag);
            if (self.isZst()) return;
            value.writeRaw(
                @ptrCast([*]u8, @ptrCast([*]u8, obj) + self.byteOffset().?),
                @intCast(u3, self.bitOffset().? - self.byteOffset().? * 8),
            );
        }

        //pub fn setDuck(self: Self, Duck: type, obj: *Duck, value: DynRecFieldValue(T)) void {}

        pub fn fromPath(path: []const []const u8) ?Self {
            var kp: ?Self = root();
            for (path) |comp| {
                if (kp != null) {
                    kp = kp.?.key(comp);
                }
            }
            return kp;
        }

        pub fn fromPathComptime(comptime path: []const []const u8) ?Self {
            for (field_data) |field| {
                if (eqlStringSlices(field.path, path)) {
                    return Self{ ._inner = @intToEnum(Inner, field.value) };
                }
            }

            return null;
        }

        pub fn eq(self: Self, other: Self) bool {
            return self._inner == other._inner;
        }

        const Int = meta.TagType(Inner);

        fn toInt(self: Self) Int {
            return @enumToInt(self._inner);
        }

        const DynType = DynRecFieldValue(T).DynType;

        const table_size = math.maxInt(Int) + 1;

        const type_table = tbl: {
            var table: [table_size]DynType = undefined;
            for (field_data) |field| {
                table[field.value] = DynRecFieldValue(T).dynType(field.field_type).?;
            }
            break :tbl table;
        };
        const NameId = math.IntFittingRange(0, names.len);
        const name_id_map: type = ComptimeStringMap(NameId, kvs: {
            comptime var _kvs: [names.len]struct { @"0": []const u8, @"1": NameId } = undefined;
            for (names) |name, i| {
                _kvs[i] = .{ .@"0" = name, .@"1" = i };
            }
            break :kvs _kvs;
        });

        const up_table: [table_size]Self = tbl: {
            var table: [table_size]Self = undefined;
            for (field_data) |field| {
                if (field.path.len == 0) continue;
                table[field.value] = Self.fromPath(field.path[0 .. field.path.len - 1]).?;
            }
            break :tbl table;
        };

        const subkey_table: [table_size][math.maxInt(NameId) + 1]?Self = tbl: {
            var table: [table_size][math.maxInt(NameId) + 1]?Self = undefined;
            for (field_data) |field| {
                for (table[field.value]) |*k| {
                    k.* = null;
                }
                switch (meta.activeTag(@typeInfo(field.field_type))) {
                    // Note that non-extern unions cannot be included, becase we won't set the tag right.
                    .Struct => {},
                    .Union => if (@typeInfo(field.field_type).Union.layout != .Extern) continue,
                    else => continue,
                }
                for (meta.fields(field.field_type)) |subfield| {
                    var name_id = name_id_map.get(subfield.name).?;
                    var subfield_path = field.path ++ [1][]const u8{subfield.name};
                    table[field.value][@as(usize, name_id)] = Self.fromPathComptime(subfield_path).?;
                }
            }
            break :tbl table;
        };

        fn dynType(self: Self) DynType {
            return type_table[@enumToInt(self._inner)];
        }

        fn isZst(self: Self) bool {
            return self.bitOffset() == null;
        }

        fn bitOffset(self: Self) ?usize {
            return TagComponents.from(keypath_data.stats, self.toInt()).offset;
        }

        fn byteOffset(self: Self) ?usize {
            if (self.bitOffset()) |bit_offset| {
                // No div by zero or overflow possible.
                return math.divFloor(usize, bit_offset, 8) catch unreachable;
            } else {
                return null;
            }
        }
    };
}

const FieldEntry = struct {
    path: []const []const u8,
    field_type: type,
    value: comptime_int,
};

const TagComponents = struct {
    offset: ?usize,
    id: usize,

    fn toTag(comptime self: TagComponents, comptime stats: TagStats) stats.Tag() {
        if (self.offset) |offset| {
            return @as(stats.Tag(), offset >> stats.aligned_bits | self.id << stats.offsetBits());
        } else {
            return @as(stats.Tag(), stats.max_sized_value + self.id);
        }
    }

    fn from(comptime stats: TagStats, tag: stats.Tag()) TagComponents {
        const offset_mask: usize = ((1 << stats.offsetBits()) - 1);
        return .{
            .offset = @as(usize, (tag & offset_mask)) << stats.aligned_bits,
            .id = tag & ~offset_mask >> stats.aligned_bits,
        };
    }
};

const TagStats = struct {
    aligned_bits: comptime_int,
    max_offset: comptime_int,
    max_sized_id: comptime_int,
    num_zst: comptime_int,

    fn offsetBits(comptime self: TagStats) comptime_int {
        return math.log2_int_ceil(usize, self.max_offset + 1) - self.aligned_bits;
    }

    fn idBits(comptime self: TagStats) comptime_int {
        return math.log2_int_ceil(usize, self.max_sized_id + self.num_zst + 1);
    }

    fn Tag(comptime self: TagStats) type {
        return meta.Int(.unsigned, self.offsetBits() + self.idBits());
    }
};

fn KeypathData(comptime T: type) type {
    return struct {
        field_data: [RecursiveField.count(T)]FieldEntry,
        stats: TagStats,
    };
}

fn buildKeypathData(comptime T: type) KeypathData(T) {
    const fields = RecursiveField.of(T);

    var field_data: [fields.len]FieldEntry = undefined;

    var tag_components: [fields.len]TagComponents = undefined;

    var next_sized_id: [@bitSizeOf(T)]comptime_int = [_]comptime_int{0} ** @bitSizeOf(T);

    var stats = TagStats{
        .aligned_bits = math.maxInt(usize),
        .max_offset = 0,
        .max_sized_id = 0,
        .num_zst = 0,
    };

    for (fields) |field, i| {
        if (@bitSizeOf(field.field_type) == 0) {
            tag_components[i] = .{ .offset = null, .id = stats.num_zst };
            stats.num_zst += 1;
        } else {
            const offset = recursiveBitOffset(T, field.path);
            tag_components[i] = .{ .offset = offset, .id = next_sized_id[offset] };
            if (next_sized_id[offset] > stats.max_sized_id) stats.max_sized_id = next_sized_id[offset];
            next_sized_id[offset] += 1;
            if (offset > stats.max_offset) stats.max_offset = offset;
            const leading = leadingBits(0, offset) orelse math.maxInt(usize);
            if (leading < stats.aligned_bits) stats.aligned_bits = leading;
        }
    }

    for (fields) |field, i| {
        field_data[i] = .{
            .path = field.path,
            .field_type = field.field_type,
            .value = tag_components[i].toTag(stats),
        };
    }

    return .{ .field_data = field_data, .stats = stats };
}

fn lexicalSort(_: void, lhs: []const u8, rhs: []const u8) bool {
    return mem.lessThan(u8, lhs, rhs);
}

fn recursiveBitOffset(comptime T: type, path: []const []const u8) comptime_int {
    if (path.len == 0) return 0;
    return @bitOffsetOf(T, path[0]) + recursiveBitOffset(
        meta.fieldInfo(T, meta.stringToEnum(meta.FieldEnum(T), path[0]).?).field_type,
        path[1..],
    );
}

fn LeadingBits(comptime T: type) type {
    if (T == comptime_int) {
        return ?comptime_int;
    } else {
        return ?math.IntFittingRange(0, meta.bitCount(T));
    }
}

fn leadingBits(bit: u1, val: anytype) LeadingBits(@TypeOf(val)) {
    if (val == 0) return null;
    var leading: LeadingBits(@TypeOf(val)) = 0;
    var _val = val;
    while (_val & 0b1 == bit) {
        leading.? += 1;
        _val >>= 1;
    }
    return leading;
}

fn mangle(comptime path: []const []const u8) []const u8 {
    // TODO: escape dots so that struct {@"a.__kp_b": usize, a: struct {b: usize}}
    // doesn't create an ambigous name.

    const sep = ".__kp_";

    comptime var name: []const u8 = "__keypath";

    comptime {
        for (path) |component| {
            name = name ++ sep ++ component;
        }
    }

    return name;
}

fn eqlStringSlices(lhs: []const []const u8, rhs: []const []const u8) bool {
    if (lhs.len != rhs.len) return false;
    for (lhs) |lhs_str, idx| {
        const rhs_str = rhs[idx];
        if (lhs_str.len != rhs_str.len) return false;
        for (lhs_str) |lhs_byte, str_idx| {
            const rhs_byte = rhs_str[str_idx];
            if (lhs_byte != rhs_byte) return false;
        }
    }
    return true;
}

test "mangling" {
    try testing.expectEqual(mangle(&.{ "a", "b", "c" }), "__keypath.__kp_a.__kp_b.__kp_c");
}

test "keypaths" {
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
        zz: Z,
    };

    var t = T{
        .x = 1,
        .y = 2,
        .z = Z{
            .a = 3,
            .b = B{ .j = 4 },
            .c = 5,
        },
        .zz = Z{
            .a = 10,
            .b = B{ .j = 40 },
            .c = 42,
        },
    };

    var kp = RecursiveKeypath(T).root();

    var x_kp = kp.key("x").?;
    var z_b_j_kp = kp.key("z").?.key("b").?.key("j").?;

    var path_kp = RecursiveKeypath(T).fromPath(&.{ "z", "b", "j" });
    try testing.expect(path_kp != null);
    try testing.expect(z_b_j_kp.eq(path_kp.?));

    try testing.expect(x_kp.get(&t).eq(DynRecFieldValue(T).of(usize, 1)));

    x_kp.set(&t, DynRecFieldValue(T).of(usize, 32));
    try testing.expect(t.x == 32);

    try testing.expect(x_kp.up().?.eq(kp));
}
