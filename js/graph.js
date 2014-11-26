var Graph = {
    Node: function (name, children) {
        var Node = Graph.Node;
        if (typeof Node.instanceId === 'undefined') {
            Node.instanceId = 0;
        }
        if (typeof Node._depthCalls === 'undefined') {
            Node._depthCalls = 0;
        }
        this._id = Node.instanceId++;
        this.name = name;
        this.children = children ? children : [];

        this.getChildren = function () {
            return this.children;
        };
        this.getName = function () {
            return this._name;
        };
        this.depth = function (nodeDepths) {
            Node._depthCalls++;
            var len = this.children.length;
            if (len == 0) {
                return 0;
            }
            if (!nodeDepths) nodeDepths = {};
            if (this._id in nodeDepths) {
                return nodeDepths[this._id];
            }
            var result = 0;
            for (var i=0; i<len; i++) {
                var d = this.children[i].depth(nodeDepths);
                if (result < d) result = d;
            }
            result++;
            nodeDepths[this._id] = result;
            return result;
        }
    },
    test: function (node) {
        var Node = node ? node : Graph.Node;
        var a = new Node("a");
        var b = new Node("b", [a]);
        var c = new Node("c", [b, a]);
        var d = new Node("d", [b, a, c]);
        var e = new Node("e", [b, a, c, d]);
        var f = new Node("f", [b, a, c, d, e]);
        var g = new Node("g", [b, a, c, d, e, f]);
        var h = new Node("h", [b, a, c, d, e, f, g]);
        var i = new Node("i", [b, a, c, d, e, f, g, h]);
        var j = new Node("j", [b, a, c, d, e, f, g, h, i]);
        var k = new Node("k", [b, a, c, d, e, f, g, h, i, j]);
        var result = "" + k.depth() + " (" + Node._depthCalls + " calls to depth())";
        return result;
    }
};

var Graph2 = {
    Node: function (name, children) {
        var Node = Graph2.Node;
        if (typeof Node._instanceId === 'undefined') {
            Node._instanceId = 0;
        }
        if (typeof Node._depthCalls === 'undefined') {
            Node._depthCalls = 0;
        }
        this._id = Node._instanceId++;
        this._name = name;
        this._children = children ? children : [];
    }
};
(function () {
    var Node = Graph2.Node;
    Node.prototype.getChildren = function () {
        return this._children;
    };
    Node.prototype.getName = function () {
        return this._name;
    };
    Node.prototype.depth = function (nodeDepths) {
        Node._depthCalls++;
        var len = this._children.length;
        if (len == 0) {
            return 0;
        }
        if (!nodeDepths) nodeDepths = {};
        if (this._id in nodeDepths) {
            return nodeDepths[this._id];
        }
        var result = 0;
        for (var i=0; i<len; i++) {
            var d = this._children[i].depth(nodeDepths);
            if (result < d) result = d;
        }
        result++;
        nodeDepths[this._id] = result;
        return result;
    };
    return true;
}());




