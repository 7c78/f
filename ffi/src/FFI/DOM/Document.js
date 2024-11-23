const _d = window.document
export {_d as document}

export function createElement(tagName) {
    return function () {
        return window.document.createElement(tagName)
    }
}

export function createTextNode(data) {
    return function () {
        return window.document.createTextNode(data)
    }
}

export function getElementById(id) {
    return function () {
        return window.document.getElementById(id)
    }
}
