
var result = [];

function foo (elem) {
    result.push(elem);
    console.log("elem:" + elem);
}

var array = [ 'a', 'b', 'c'];

// Test to see whether we need to define a function to be invoked as part of
// each iteration in the forEach. Of course we don
array.forEach(foo);
array.forEach(function (elem) {
    foo(elem)
});



