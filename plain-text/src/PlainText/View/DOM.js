export function isText(node) {
    return node instanceof window.Text
}

export function isElement(node) {
    return node instanceof window.Element
}

export function eqDOMNode(x) {
    return function (y) {
        return x === y
    }
}
