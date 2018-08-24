/**
 * This file contains code that implements features on old web browsers that do not support these
 *  features. Such code is needed due to the difference in API of new beowsers and IE.
 */

// WheelEvent polyfill
(function (window) {
    try {
        new window.WheelEvent('test');
        return false; // No need to polyfill
    } catch (e) {
      // Need to polyfill - fall through
    }

    var keyModifierMapping = {
        altKey: 'Alt',
        ctrlKey: 'Control',
        metaKey: 'Meta',
        shiftKey: 'Shift',
        modifierAltGraph: 'AltGraph',
        modifierCapsLock: 'CapsLock',
        modifierFn: 'Fn',
        modifierFnLock: 'FnLock',
        modifierHyper: 'Hyper',
        modifierNumLock: 'NumLock',
        modifierScrollLock: 'ScrollLock',
        modifierSuper: 'Super',
        modifierSymbol: 'Symbol',
        modifierSymbolLock: 'SymbolLock',
    };

    var keyModifiers = Object.getOwnPropertyNames(keyModifierMapping);

    function getKeyModifierList(props) {
        var list = [];
        for (var i = 0; i < keyModifiers.length; ++i) {
            var prop = keyModifiers[i];
            if (props[prop]) {
                list.push(keyModifierMapping[prop]);
            }
        }
        return list.join(' ');
    }
  
    var WheelEvent = function (eventType, params) {
        params = params || { 
            bubbles: false, 
            cancelable: false,
            detail: 0,
            screenX: 0,
            screenY: 0,
            clientX: 0,
            clientY: 0,
            button: 0,
            relatedTarget: null,
            deltaX: 0,
            deltaY: 0,
            deltaZ: 0,
            deltaMode: 0
        };

        var wheelEvent = document.createEvent('WheelEvent');
        wheelEvent.initWheelEvent(
            eventType,
            params.bubbles,
            params.cancelable,
            window,
            params.detail,
            params.screenX,
            params.screenY,
            params.clientX,
            params.clientY,
            params.button,
            params.relatedTarget,
            getKeyModifierList(params),
            params.deltaX,
            params.deltaY,
            params.deltaZ,
            params.deltaMode
        );
  
        return wheelEvent;
    };
  
    WheelEvent.prototype = Event.prototype;
  
    window.WheelEvent = WheelEvent;
})(window);

// Helper function to clone events. Usage: Event.clone(e)
(function (window) {
    if (window.Event.clone) {
        return false;
    }

    window.Event.clone = function (event) {
        if (!event) return null;

        if (event.type === 'wheel') {
            return new window.WheelEvent(event.type, event);
        } else {
            console.warn('Event.clone(event) function doesn\'t support', event.type, 'event');
            return null;
        }
    };
})(window);

// Array polyfill
(function () {
    // https://tc39.github.io/ecma262/#sec-array.prototype.find
    if (!Array.prototype.find) {
        Object.defineProperty(Array.prototype, 'find', {
            value: function (predicate) {
                // 1. Let O be ? ToObject(this value).
                if (this == null) {
                    throw new TypeError('"this" is null or not defined');
                }

                var o = Object(this);

                // 2. Let len be ? ToLength(? Get(O, "length")).
                var len = o.length >>> 0;

                // 3. If IsCallable(predicate) is false, throw a TypeError exception.
                if (typeof predicate !== 'function') {
                    throw new TypeError('predicate must be a function');
                }

                // 4. If thisArg was supplied, let T be thisArg; else let T be undefined.
                var thisArg = arguments[1];

                // 5. Let k be 0.
                var k = 0;

                // 6. Repeat, while k < len
                while (k < len) {
                    // a. Let Pk be ! ToString(k).
                    // b. Let kValue be ? Get(O, Pk).
                    // c. Let testResult be ToBoolean(? Call(predicate, T, « kValue, k, O »)).
                    // d. If testResult is true, return kValue.
                    var kValue = o[k];
                    if (predicate.call(thisArg, kValue, k, o)) {
                        return kValue;
                    }
                    // e. Increase k by 1.
                    k++;
                }

                // 7. Return undefined.
                return undefined;
            }
        });
    }
})();
