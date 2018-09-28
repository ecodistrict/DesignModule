/**
 * PerimeterLayout places window objects along the window perimeter 
 * leaving the center are of a window free.
 */

/* globals L */ 

var PerimeterLayout = L.Evented.extend({
    initialize: function (opts) {
        this._options = L.extend({
            padding: {
                left: 0,
                right: 0,
                top: 0,
                bottom: 0
            },
            verticalCellSpacing: 5,
            horizontalCellSpacing: 5,
            topRowCellCount: 2,
            leftColumnCellCount: 4,
            rightColumnCellCount: 4,
            maxCellWidth: 400,
            maxCellHeight: 300,
            minCellWidth: 180,
            minCellHeight: 100
        }, opts);

        this._size = { width: 0, height: 0 };
        this._buildCells();
    },

    resize: function (size) {
        this._size = size;
        this._resizeCells();
    },

    findNearestSpace: function (layoutItem) {
        if (this._cells.length === 0) return null;

        var distance = this._distance.bind(this);

        var nearestCell = this._cells[0];
        var minDistance = this._distance(layoutItem, nearestCell);
        this._cells.forEach(function (cell) {
            var d = distance(layoutItem, cell);
            if (d < minDistance) {
                minDistance = d;
                nearestCell = cell;
            }
        });

        return {
            index: nearestCell.index,
            geometry: nearestCell.geometry
        };
    },

    findFreeSpace: function (/* jshint unused:false */ layoutItem) {
        var cell = this._cells.find(function (cell) { return !cell.layoutItem; });
        if (!cell) return null;

        return {
            index: cell.index,
            geometry: cell.geometry
        };
    },

    getLayoutItem: function (space) {
        if (!space) return null;
        if (space.index < 0 || space.index >= this._cells.length) return null;

        return this._cells[space.index].layoutItem;
    },

    getLayoutItemSpace: function (layoutItem) {
        var cell = this._cells.find(function (cell) { 
            return cell.layoutItem === layoutItem; 
        });
        if (!cell) return null;

        return {
            index: cell.index,
            geometry: cell.geometry
        };
    },

    insert: function (layoutItem, space) {
        var spaceToInsert = space || this.findFreeSpace(layoutItem);
        if (!spaceToInsert) return false;
        if (spaceToInsert.index < 0 || spaceToInsert.index >= this._cells.length) return false;

        var cell = this._cells[spaceToInsert.index];
        cell.layoutItem = layoutItem;
        layoutItem.fillSpace(cell.geometry);

        this._notifyFreeSpaceStatus(this.hasFreeSpace());

        return true;
    },

    remove: function (layoutItem) {
        var cell = this._cells.find(function (cell) { 
            return cell.layoutItem === layoutItem;
        });
        if (!cell) return;

        cell.layoutItem = null;

        this._notifyFreeSpaceStatus(this.hasFreeSpace());
    },

    hasFreeSpace: function () {
        return !!this.findFreeSpace();
    },

    _buildCells: function () {        
        this._cells = [];

        var addCell = function(location) {
            this._cells.push({ 
                layoutItem: null, 
                index: this._cells.length,
                location: location
            });
        }.bind(this);

        var i;
        for (i = 0; i < this._options.leftColumnCellCount; ++i) {
            addCell('left');
        }
        for (i = 0; i < this._options.rightColumnCellCount; ++i) {
            addCell('right');
        }
        for (i = 0; i < this._options.topRowCellCount; ++i) {
            addCell('top');
        }
    },

    _resizeCells: function () {
        var layoutWidth = this._size.width;
        var cellWidth = this._calculateCellWidth();
        var cellHeight = this._calculateCellHeight();

        var verticalCellSpacing = this._options.verticalCellSpacing;
        var horizontalCellSpacing = this._options.horizontalCellSpacing;

        var padding = this._options.padding;

        var leftColumn = this._cells.filter(function (cell) { return cell.location === 'left'; });
        var rightColumn = this._cells.filter(function (cell) { return cell.location === 'right'; });
        var topRow = this._cells.filter(function (cell) { return cell.location === 'top'; });

        leftColumn.forEach(function (cell, i) {
            cell.geometry = {
                x: padding.left,
                y: padding.top + (cellHeight + verticalCellSpacing) * i,
                width: cellWidth,
                height: cellHeight
            };

            if (cell.layoutItem) {
                cell.layoutItem.fillSpace(cell.geometry);
            }
        });

        rightColumn.forEach(function (cell, i) {
            cell.geometry = {
                x: layoutWidth - padding.right - cellWidth,
                y: padding.top + (cellHeight + verticalCellSpacing) * i,
                width: cellWidth,
                height: cellHeight
            };

            if (cell.layoutItem) {
                cell.layoutItem.fillSpace(cell.geometry);
            }
        });

        topRow.forEach(function (cell, i) {
            cell.geometry = {
                x: padding.left + (cellWidth + horizontalCellSpacing) * (i + 1),
                y: padding.top,
                width: cellWidth,
                height: cellHeight
            };

            if (cell.layoutItem) {
                cell.layoutItem.fillSpace(cell.geometry);
            }
        });
    },

    _calculateCellWidth: function () {
        var leftColumn = this._cells.filter(function (cell) { return cell.location === 'left'; });
        var rightColumn = this._cells.filter(function (cell) { return cell.location === 'right'; });
        var topRow = this._cells.filter(function (cell) { return cell.location === 'top'; });

        var horizontalCellCount = topRow.length + 
            (leftColumn.length > 0 ? 1 : 0) + 
            (rightColumn.length > 0 ? 1 : 0);

        var horizontalCellSpacing = this._options.horizontalCellSpacing;
        var padding = this._options.padding;

        var horizontalOffsetSpace = padding.left + padding.right + 
            (horizontalCellCount - 1) * horizontalCellSpacing;
        var cellWidth = (this._size.width - horizontalOffsetSpace) / horizontalCellCount;
        
        if (this._options.maxCellWidth) {
            cellWidth = Math.min(cellWidth, this._options.maxCellWidth);
        }
        if (this._options.minCellWidth) {
            cellWidth = Math.max(cellWidth, this._options.minCellWidth);
        }

        return cellWidth;        
    },

    _calculateCellHeight: function () {
        var leftColumn = this._cells.filter(function (cell) { return cell.location === 'left'; });
        var rightColumn = this._cells.filter(function (cell) { return cell.location === 'right'; });

        var maxVerticalCellCount = Math.max(leftColumn.length, rightColumn.length);
        var verticalCellSpacing = this._options.verticalCellSpacing;
        var padding = this._options.padding;

        var verticalOffsetSpace = padding.top + padding.botton + 
            (maxVerticalCellCount - 1) * verticalCellSpacing;
        var cellHeight = (this._size.height - verticalOffsetSpace) / maxVerticalCellCount;
        
        if (this._options.maxCellHeight) {
            cellHeight = Math.min(cellHeight, this._options.maxCellHeight);
        }
        if (this._options.minCellHeight) {
            cellHeight = Math.max(cellHeight, this._options.minCellHeight);
        }

        return cellHeight;
    },

    _distance: function (layoutItem, cell) {
        var layoutItemRect = layoutItem.getBoundingRect();
        var cellRect = cell.geometry;
        
        var layoutItemCenter = {
            x: layoutItemRect.x + layoutItemRect.width / 2,
            y: layoutItemRect.y + layoutItemRect.height / 2
        };
        var cellCenter = {
            x: cellRect.x + cellRect.width / 2,
            y: cellRect.y + cellRect.height / 2
        };

        return Math.sqrt(Math.pow(layoutItemCenter.x - cellCenter.x, 2) + 
                         Math.pow(layoutItemCenter.y - cellCenter.y, 2));
    },

    _notifyFreeSpaceStatus: function (freeSpaceAvailable) {
        this.fire('freeSpaceStatus', { freeSpaceAvailable: freeSpaceAvailable });
    }

});

export default PerimeterLayout;
