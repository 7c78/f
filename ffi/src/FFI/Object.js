export function unsafeGetProperty(obj) {
    return function (prop) {
        return obj[prop]
    }
}

export function applyMethod(obj) {
    return function (prop) {
        return function (args) {
            return obj[prop].apply(obj, args)
        }
    }
}

export const args0 = []
export function _args1(x1) { return [x1] }
export function _args2(x1, x2) { return [x1, x2] }
export function _args3(x1, x2, x3) { return [x1, x2, x3] }
export function _args4(x1, x2, x3, x4) { return [x1, x2, x3, x4] }
export function _args5(x1, x2, x3, x4, x5) { return [x1, x2, x3, x4, x5] }

function _typeof(v) {
    return typeof v
}
export { _typeof as typeof }

function _instanceof(v) {
    return function (ctor) {
        return v instanceof ctor
    }
}
export { _instanceof as instanceof }
