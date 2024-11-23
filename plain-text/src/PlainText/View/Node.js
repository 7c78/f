function setDOMx(node, data) {
    node.$$x = data
}
export {setDOMx as _setDOMx}

export function emptyTextViewNode(node) {
    return function (domNode) {
        return {
            node,
            domNode,
            text: true
        }
    }
}

export function textViewNode(node) {
    return function (domNode) {
        return function (domText) {
            return {
                node,
                domNode,
                domText,
                text: true
            }
        }
    }
}

export function updateTextViewNode(vnode) {
    return function (node) {
        return {...vnode, node}
    }
}

function emptyText({node, offset}) {
    if (offset > 0)
        return null
    if (node.nodeName === "BR" && node.$$x?.text === true)
        return node.$$x
    if (node.childNodes.length === 1) {
        let child = node.childNodes[0]
        if (child.nodeName === "BR" && child.$$x?.text === true)
            return child.$$x
    }
    return null
}
export {emptyText as _emptyText}

export function domText(vnode) {
    return vnode.domNode.nodeName === "BR"
         ? vnode.domNode
         : vnode.domText
}

function closest(rootNode, node, match) {
    if (node == null)
        return null
    if (node.$$x?.node != null && match(node.$$x.node))
        return node.$$x
    if (node !== rootNode)
        return closest(rootNode, node.parentNode, match)
    return null
}
export {closest as _closest}

function getRenderedChildren(node) {
    return node.$$x == null
         ? []
         : node.$$x.children
}
export {getRenderedChildren as _getRenderedChildren}

function setDOMTextData(node, s) {
    node.$$x.domText.data = s
}
export {setDOMTextData as _setDOMTextData}
