// https://developer.mozilla.org/en-US/docs/Web/API/Document/defaultView
export function getWindow(n) {
    return n.ownerDocument?.defaultView ?? window
}

// https://developer.mozilla.org/en-US/docs/Web/API/Node/ownerDocument
export function getDocument(n) {
    return n.ownerDocument ?? document
}

export function isNode(x) {
    let w = getWindow(x)
    return x instanceof w.Node
}

export function isElement(x) {
    let w = getWindow(x)
    return x instanceof w.Element
}

export function isHTMLElement(x) {
    let w = getWindow(x)
    return x instanceof w.HTMLElement
}

export function isHtml(n) {
    return n.nodeName === "HTML"
}

export function isBody(n) {
    return n.nodeName === "BODY"
}

// https://developer.mozilla.org/en-US/docs/Web/API/Document/documentElement
export function getHtml(d) {
    return d.documentElement
}

// https://developer.mozilla.org/en-US/docs/Web/API/Document/body
export function getBody(d) {
    return d.body
}

// https://developer.mozilla.org/en-US/docs/Web/CSS/Viewport_concepts
// https://javascript.info/size-and-scroll-window#width-height-of-the-window
function getViewportRect(e) {
    let html = getHtml(getDocument(e))
    return toRect({
        width: html.clientWidth,
        height: html.clientHeight,
        top: 0,
        left: 0
    })
}
export {getViewportRect as _getViewportRect}

// https://developer.mozilla.org/en-US/docs/Web/API/Element/getBoundingClientRect
function getElementRect(e) {
    return e.getBoundingClientRect()
}
export {getElementRect as _getElementRect}

// https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Determining_the_dimensions_of_elements
function getElementDimensions(e) {
    let {width, height} = getElementRect(e)
    return {width, height}
}
export {getElementDimensions as _getElementDimensions}

// Return the client rect without scrollbars of an element
function getElementInnerRect(e) {
    let {left, top} = e.getBoundingClientRect()
    return toRect({
        left,
        top,
        width: e.clientWidth,
        height: e.clientHeight
    })
}
export {getElementInnerRect as _getElementInnerRect}

// https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollLeft
// https://developer.mozilla.org/en-US/docs/Web/API/Element/scrollTop
function getElementScroll(e) {
    return {
        scrollLeft: e.scrollLeft,
        scrollTop: e.scrollTop
    }
}
export {getElementScroll as _getElementScroll}

// https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollX
// https://developer.mozilla.org/en-US/docs/Web/API/Window/scrollY
function getWindowScroll(w) {
    return {
        scrollLeft: w.scrollX,
        scrollTop: w.scrollY
    }
}
export {getWindowScroll as _getWindowScroll}

// https://developer.mozilla.org/en-US/docs/Web/API/Window/getComputedStyle
function getComputedStyle(e) {
    let w = getWindow(e)
    return w.getComputedStyle(e)
}
export {getComputedStyle as _getComputedStyle}

// https://developer.mozilla.org/en-US/docs/Web/CSS/overflow
function isOverflowElement(e) {
    if (!isHTMLElement(e))
        return false

    let {display, overflow, overflowX, overflowY} = getComputedStyle(e)
    if (["inline", "contents"].includes(display))
        return false

    let value = ["auto", "scroll", "overlay", "hidden", "clip"]
    return value.includes(overflow)
        || value.includes(overflowX)
        || value.includes(overflowY)
}
export {isOverflowElement as _isOverflowElement}

function getImmediateOverflowParent(e) {
    if (isHtml(e) || isBody(e))
        return null

    let parent = e.parentElement
    return isOverflowElement(parent)
        ? parent
        : getImmediateOverflowParent(parent)
}
export {getImmediateOverflowParent as _getImmediateOverflowParent}

function getOverflowParents(e) {
    return loop([], e)

    function loop(z, x) {
        let parent = getImmediateOverflowParent(x)
        if (parent == null)
            return z

        return loop([...z, parent], parent)
    }
}
export {getOverflowParents as _getOverflowParents}

// https://developer.mozilla.org/en-US/docs/Learn/CSS/CSS_layout/Positioning#positioning_contexts
// https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block
//
// For absolutely placed elements, the containing block is the nearest ancestor element that has one
// of the following properties:
function isContainingBlock(e) {
    let {filter, backdropFilter, transform, perspective
        ,contain
        ,containerType
        ,willChange} = getComputedStyle(e)

    return (  filter !== "none"
           || backdropFilter !== "none"
           || transform !== "none"
           || perspective !== "none")
        || ["layout", "paint", "strict", "content"].includes(contain)
        || containerType !== "normal"
        || ["transform", "perspective", "filter"].includes(willChange)
}
export {isContainingBlock as _isContainingBlock}

// https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetParent
function getOffsetParent(e) {
    if (!isHTMLElement(e))
        return null

    if (getComputedStyle(e).position === "fixed")
        return null

    let parent = e.offsetParent

    if (   parent != null
        && isBody(parent)
        && getComputedStyle(parent).position === "static"
        && !isContainingBlock(parent)
    ) {
        return null
    }

    return parent
}
export {getOffsetParent as _getOffsetParent}

function toRect(rect) {
    return {
        ...rect,
        right: rect.left + rect.width,
        bottom: rect.top + rect.height
    }
}

// https://developer.mozilla.org/en-US/docs/Web/API/Document/scroll_event#scroll_event_throttling
function onScroll(node, listener) {
    let ticking = false

    node.addEventListener("scroll", (event) => {
        if (!ticking) {
            window.requestAnimationFrame(() => {
                listener(event)
                ticking = false
            })

            ticking = true
        }
    })
}
export {onScroll as _onScroll}
