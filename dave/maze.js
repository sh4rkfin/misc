var maze = {
    Node: function (rowId, colId, neighbors) {
        var Node = maze.Node;
        if (typeof Node._instanceId === 'undefined') {
            Node._instanceId = 0;
        }
        this._id = Node._instanceId++;
        this._name = "" + rowId + "-" +colId;
        this._rowId = rowId;
        this._colId = colId;
        this._neighbors = neighbors ? neighbors : [];
    },
    Maze: function (rows, columns) {
        this._nodes = [];
        var Maze = maze.Maze;
        var Node = maze.Node;
        this._rows = rows;
        this._cols = columns;
        this._nodesByName = {};
        this._sourceId = null;
        this._destId = null;
        for (var k=0; k < rows; k++) {
            this._nodes[k] = [];
            for (var i=0; i < columns; i++) {
                var node = new Node(k, i);
                this._nodes[k][i] = node;
                this._nodesByName[node.name()] = node;
            }
        }
        for (var k=0; k < rows; k++) {
            for (var i=0; i < columns; i++) {
                for (var dir in Maze.dirs) {
                    var rnd = Math.random();
                    if (rnd < 0.35) {
                        var node = this._nodes[k][i];
                        var next = Maze.dirs[dir].get.call(this, node);
                        if (next) {
                            node.addLink(next);
                        }
                    }
                }
            }
        }
    }
};
(function () {
    var Node = maze.Node;
    Node.prototype.neighbors = function () {
        return this._neighbors;
    };

    Node.prototype.addLink = function (node, dontAddBack) {
        if (this._neighbors.indexOf(node) < 0) {
            this._neighbors.push(node);
        }
        if (!dontAddBack) {
            node.addLink(this, true);
        }
    };

    Node.prototype.rowId = function (node) {
        return this._rowId;
    };
    Node.prototype.colId = function (node) {
        return this._colId;
    };
    Node.prototype.name = function (node) {
        return this._name;
    };
    var Maze = maze.Maze;
    Maze.prototype.north = function (node) {
        var rid = node.rowId();
        return rid > 0 ? this._nodes[rid - 1][node.colId()] : null;
    };
    Maze.prototype.south = function (node) {
        var rid = node.rowId();
        return rid < this._rows - 1 ? this._nodes[rid + 1][node.colId()] : null;
    };
    Maze.prototype.east = function (node) {
        var cid = node.colId();
        return cid > 0 ? this._nodes[node.rowId()][cid - 1] : null;
    };
    Maze.prototype.west = function (node) {
        var cid = node.colId();
        return cid < this._cols - 1 ? this._nodes[node.rowId()][cid + 1] : null;
    };
    Node.prototype.isNorthOf = function (node) {
        return this.rowId() < node.rowId();
    };
    Node.prototype.isSouthOf = function (node) {
        return this.rowId() > node.rowId();
    };
    Node.prototype.isEastOf = function (node) {
        return this.colId() > node.colId();
    };
    Node.prototype.isWestOf = function (node) {
        return this.colId() < node.colId();
    };
    Maze.prototype.getArcs = function () {
        var result = "";
        for (var i=0; i<this._nodes.length; i++) {
            var row = this._nodes[i];
            for (var j=0; j<row.length; j++) {
                var cell = row[j];
                var neighbors = cell.neighbors();
                for (var k=0; k<neighbors.length; k++) {
                    var nbor = neighbors[k];
                    result += cell.name() + " -> " + nbor.name() + "<br/>"
                }
            }
        }
        return result;
    };
    Maze.makeDirs = function () {
        return { north: null, south: null, east: null, west: null };
    };
    Maze.dirs = {
        north: {
            get: Maze.prototype.north,
            isInDirection: Node.prototype.isNorthOf,
            wallStyle: "btop"
        },
        south: {
            get: Maze.prototype.south,
            isInDirection: Node.prototype.isSouthOf,
            wallStyle: "bbottom"
        },
        east: {
            get: Maze.prototype.east,
            isInDirection: Node.prototype.isEastOf,
            wallStyle: "bright"
        },
        west: {
            get: Maze.prototype.west,
            isInDirection: Node.prototype.isWestOf,
            wallStyle: "bleft"
        }
    };
    Maze.dirlist = [Maze.dirs.north,
                    Maze.dirs.south,
                    Maze.dirs.east,
                    Maze.dirs.west];
    Maze.borderClasses = ["btop", "bleft", "bright", "bbottom"];
    Maze.prototype.sourceId = function () {
        return this._sourceId;
    };
    Maze.prototype.destId = function () {
        return this._destId;
    };
    Maze.prototype.setSource = function (sourceId) {
        this._sourceId = sourceId;
    };
    Maze.prototype.setDest = function (id) {
        this._destId = id;
    };
    Maze.prototype.calculateRoute = function () {
        var source = this._nodesByName[this._sourceId];
        var dest = this._nodesByName[this._destId];
        var sentinel = {};
        var nodes = [source, sentinel];
        var result = {result:"failure"};
        result[source.name()] = {dist:0, parent:null, visited:false};
        var dist = 0;
        while (nodes.length > 0) {
            var current = nodes.shift();
            if (current === sentinel) {
                if (nodes.length > 0) {
                    nodes.push(sentinel);
                }
                dist++;
            }
            else if (current === dest) {
                result['result'] = "success";
                break;
            }
            else {
                result[current.name()].visited = true;
                var nbors = current.neighbors();
                for (var j=0; j<nbors.length; j++) {
                    var nbor = nbors[j];
                    if (!result[nbor.name()]) {
                        nodes.push(nbor);
                        result[nbor.name()] = {dist: (dist + 1), parent: current.name(), visited:false};
                    }
                }
            }
        }
        return result;
    };
    Maze.prototype.draw = function ()
    {
        for (var k=0; k < this._rows; k++) {
            var row = document.createElement("tr");
            row.setAttribute("id", "r" + k);
            $( ".mazeBody" ).append(row);

            for (var i=0; i<this._cols; i++) {
                var node = this._nodes[k][i];
                cell = document.createElement("td");
                cell.setAttribute("id", node.name());
                row.appendChild(cell);

                var neighbors = node.neighbors();
                var dirs = Maze.makeDirs();
                for (var j=0; j<neighbors.length; j++) {
                    var neighbor = neighbors[j];
                    for (var dir in Maze.dirs) {
                        if (Maze.dirs[dir].isInDirection.call(neighbor, node)) {
                            dirs[dir] = true;
                            break;
                        }
                    }
                }
                var cls = " ";
                for (var dir in dirs) {
                    if (!dirs[dir]) {
                        cls = cls + Maze.dirs[dir].wallStyle + " ";
                    }
                }
                //cell.innerHTML = node.name();
                cell.setAttribute("class", cls);
            }
        }
    };
}());


function drawRandomMaze(rows, columns)
{
    for (var k=0; k < rows; k++) {
        var row = document.createElement("tr");
        row.setAttribute("id", "r" + k);
        $( ".mazeBody" ).append(row);

        for (var i=0; i<45; i++) {
            cell = document.createElement("td");
            var id = "" + k + "-" + i;
            cell.setAttribute("id", id);
            row.appendChild(cell);
            var cls = " ";
            for (var j=0; j<4; j++) {
                if (Math.random() < 0.3) {
                    cls = cls + maze.Maze.borderClasses[j] + " ";
                }
            }
            cell.setAttribute("class", cls);
        }
    }
}

function testFor () {
    for (var dir in maze.Maze.dirs) {
        var foo = dir;
    }
}

function testFunction () {
    var foo = maze.Node.prototype.isNorthOf;
    var bar = foo;
}

function test () {
    testFunction();
}



