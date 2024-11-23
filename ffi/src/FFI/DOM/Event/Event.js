export function stopPropagation(e) {
    return function () {
        e.stopPropagation()
    }
}

export function stopImmediatePropagation(e) {
    return function () {
        e.stopImmediatePropagation()
    }
}

export function preventDefault(e) {
    return function () {
        e.preventDefault()
    }
}
