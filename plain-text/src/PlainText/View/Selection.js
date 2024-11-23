import { isText, isElement } from "@PlainText/View/DOM"

function normalizeDOMPoint(point) {
    return normalizeBackward(point)
        ?? normalizeForward(point)
}
export {normalizeDOMPoint as _normalizeDOMPoint}

function normalizeForward({node, offset}) {
    if (isText(node))
        return {node, offset}

    if (isElement(node)) {
        for (let i = offset; i < node.childNodes.length; i = i + 1) {
            let child = node.childNodes[i]
            let normalized = normalizeForward({node: child, offset: 0})
            if (normalized != null) {
                return normalized
            }
        }
    }

    return null
}

function normalizeBackward({node, offset}) {
    if (isText(node))
        return {node, offset}

    if (isElement(node)) {
        for (let i = offset - 1; i >= 0; i = i - 1) {
            let child = node.childNodes[i]
            let offset = isText(child)
                        ? child.length
                        : child.childNodes.length
            let normalized = normalizeBackward({node: child, offset})
            if (normalized != null) {
                return normalized
            }
        }
    }

    return null
}

function selectionWithin(sel, elem) {
    if (sel == null || sel.anchorNode == null || sel.focusNode == null)
        return false
    return elem.contains(sel.anchorNode)
        && elem.contains(sel.focusNode)
}
export {selectionWithin as _selectionWithin}

function renderDOMSelection(sel, anchor, focus) {
    let equal = sel.anchorNode === anchor.node
             && sel.anchorOffset === anchor.offset
             && sel.focusNode === focus.node
             && sel.focusOffset === focus.offset
    if (!equal) {
        sel.setBaseAndExtent(
            anchor.node,
            anchor.offset,
            focus.node,
            focus.offset
        )
    }
}
export {renderDOMSelection as _renderDOMSelection}

export function getSelection() {
    return window.document.getSelection()
}

export function getAnchor(sel) {
    return {node: sel.anchorNode, offset: sel.anchorOffset}
}

export function getFocus(sel) {
    return {node: sel.focusNode, offset: sel.focusOffset}
}

export function isCollapsed(sel) {
    return sel.isCollapsed
}

function clearDOMSelection(sel) {
    sel.removeAllRanges()
}
export {clearDOMSelection as _clearDOMSelection}
