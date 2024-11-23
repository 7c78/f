export function insertProperties(obj) {
    return function (kvs) {
        return function () {
            Object.assign(obj, kvs)
        }
    }
}

export function deleteProperty(obj) {
    return function (k) {
        return function () {
            delete obj[k]
        }
    }
}
