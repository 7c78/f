// mutable associative array

export function newSTAssocArray() {
    return {}
}

export function insertSTAssocArray(key) {
    return function (val) {
        return function (obj) {
            return function () {
                obj[key] = val
                return obj
            }
        }
    }
}

export function deleteSTAssocArray(key) {
    return function (obj) {
        return function () {
            delete obj[key]
            return obj
        }
    }
}

export function _copyST(x) {
    return function () {
        let r = {}
        for (let [k, v] of Object.entries(x)) {
            r[k] = v
        }
        return r
    }
}

export function runST(f) {
    return f()
}


// immutable associative array

export const empty = {}

export function _equal(x) {
    return function (y) {
        let xkeys = Object.keys(x)
        let ykeys = Object.keys(y)
        if (xkeys.length !== ykeys.length) {
            return false
        }
        for (let k of xkeys) {
            if (!Object.hasOwn(y, k) || x[k] !== y[k]) {
                return false
            }
        }
        return true
    }
}

export function _foldM(bind) {
    return function (f) {
        return function (mz) {
            return function (obj) {
                let acc = mz
                function g(k) {
                    return function (z) {
                        return f(z)(k)(obj[k])
                    }
                }
                for (let k of Object.keys(obj)) {
                    acc = bind(acc)(g(k))
                }
                return acc
            }
        }
    }
}

export function unsafeLookup(obj) {
    return function (key) {
        return Object.hasOwn(obj, key)
                ? obj[key]
                : null
    }
}

export const keys = Object.keys

export const elems = Object.values

export function member(key) {
    return function (obj) {
        return Object.hasOwn(obj, key)
    }
}
