/**
 * Utility functions to work with the DOM tree.
 */

/* exported DomUtil */
var DomUtil = {
    calculateTextSize: function (node, text) {
        var font = window.getComputedStyle(node, null).getPropertyValue('font-family');
        var fontSize = window.getComputedStyle(node, null).getPropertyValue('font-size');
        var canvas = document.createElement('canvas');
        var context = canvas.getContext('2d');
        context.font = fontSize + ' ' + font;
        
        var textSize = context.measureText(text);
        textSize.height = parseInt(fontSize, 10) || 10;

        return textSize;
    },

    truncateTextByWidth: function (node, text, width) {
        var textWidht = DomUtil.calculateTextSize(node, text).width;
        if (textWidht <= width) return text;            

        var ending = '...';
        var newText = '';
        var letterIndex = 0;
        while (
            letterIndex < text.length &&
            DomUtil.calculateTextSize(node, newText + text[letterIndex] + ending).width <= width
        ) {
            newText += text[letterIndex];
            ++letterIndex;
        }

        return newText + ending;
    }
};
