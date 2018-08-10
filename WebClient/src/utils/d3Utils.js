/**
 * Set of utility function that can be used together with d3.
 */

/* exported D3Utils */
var D3Utils = {
    calculateTextWidth: function (text) {
        var font = window.getComputedStyle(this, null).getPropertyValue('font-family');
        var fontSize = window.getComputedStyle(this, null).getPropertyValue('font-size');
        var canvas = document.createElement('canvas');
        var context = canvas.getContext('2d');
        context.font = fontSize + ' ' + font;

        return context.measureText(text).width;
    },

    truncateTextByWidth: function (width) {
        return function (text) {
            var textWidht = D3Utils.calculateTextWidth.call(this, text);
            if (textWidht <= width) return text;            

            var ending = '...';
            var newText = '';

            var calculateWidth = function(letter) {
                return D3Utils.calculateTextWidth.call(this, newText + letter + ending);
            }.bind(this);

            var letterIndex = 0;
            while (
                letterIndex < text.length &&
                calculateWidth(text[letterIndex]) <= width
            ) {
                newText += text[letterIndex];
                ++letterIndex;
            }

            return newText + ending;
        };
    }
};
