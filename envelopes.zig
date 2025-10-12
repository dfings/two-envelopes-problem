//! $ brew install zig
//! $ zig run envelopes.zig
//! or
//! $ zig build-exe envelopes.zig
//! $ ./envelopes

const std = @import("std");

const NUM_TRIALS: u32 = 10000;
const PRIOR_LOWER_MAX: f64 = 100.0;

/// Runs a single trial where an envelope is chosen.
///
/// If the chosen envelope has a value < cutoff, the function will switch
/// envelopes; otherwise, it will keep the envelope it has chosen. Returns
/// the value of the envelope it ultimately selects.
fn single_trial(random: std.Random, cutoff: f64) f64 {
    const lower_value = random.float(f64) * PRIOR_LOWER_MAX;
    const higher_value = 2.0 * lower_value;

    // Randomly choose which envelope to inspect first (true = lower, false = higher).
    if (random.boolean()) {
        // Inspected the envelope with the lower value first.
        // Switch if its value is less than the cutoff.
        return if (lower_value >= cutoff) lower_value else higher_value;
    } else {
        // Inspected the envelope with the higher value first.
        // Switch if its value is less than the cutoff.
        return if (higher_value >= cutoff) higher_value else lower_value;
    }
}

/// Runs many trials at a given cutoff to approximate the expected value.
fn multi_trial(random: std.Random, cutoff: f64) f64 {
    var total: f64 = 0.0;
    var i: u32 = 0;
    while (i < NUM_TRIALS) : (i += 1) {
        total += single_trial(random, cutoff);
    }
    return total / @as(f64, NUM_TRIALS);
}

/// Approximates the expected value for each integral cutoff value.
pub fn main() !void {
    // 1. Seed the Pseudo-Random Number Generator (PRNG).
    var prng = std.Random.Sfc64.init(blk: {
        var seed: u64 = undefined;
        try std.posix.getrandom(std.mem.asBytes(&seed));
        break :blk seed;
    });
    const random = prng.random();

    // 3. Loop through cutoff values and print the results.
    const MAX_CUTOFF = @as(u32, @floor(2.0 * PRIOR_LOWER_MAX));
    var cutoff: u32 = 0;
    while (cutoff <= MAX_CUTOFF) : (cutoff += 1) {
        const cutoff_f64 = @as(f64, @floatFromInt(cutoff));
        const expected_value = multi_trial(random, cutoff_f64);
        std.debug.print("cutoff={d}, expected_value={d}\n", .{ cutoff, expected_value });
    }
}
