export function addEventListener(target) {
    return function (etype) {
        return function (listener) {
            return function () {
                target.addEventListener(etype, listener)
            }
        }
    }
}

export function removeEventListener(target) {
    return function (etype) {
        return function (listener) {
            return function () {
                target.removeEventListener(etype, listener)
            }
        }
    }
}
