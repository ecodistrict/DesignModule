/**
 * Utilities that adds more features when working with javascript functions.
 */

export function not(predicate) {
    return function() {
        return !predicate.apply(undefined, arguments);
    };
}
