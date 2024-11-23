export function append(elem) {
    return function (child) {
        return function () {
            elem.append(child)
        }
    }
}

export function before(elem) {
    return function (sibling) {
        return function () {
            elem.before(sibling)
        }
    }
}

export function after(elem) {
    return function (sibling) {
        return function () {
            elem.after(sibling)
        }
    }
}

export function replaceWith(elem) {
    return function (newElem) {
        return function () {
            elem.replaceWith(newElem)
        }
    }
}

export function remove(elem) {
    return function () {
        elem.remove()
    }
}
