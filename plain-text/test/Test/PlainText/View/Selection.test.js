import { describe, it, assert } from "vitest"
import { _normalizeDOMPoint } from "@PlainText/View/Selection"

function sandbox(html) {
    let div = document.createElement("div")
    div.innerHTML = html.trim()
    return div
}

describe("normalizeDOMPoint", () => {
    it("return the original point if the node is a text node", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let t0 = p.childNodes[0]
        let normalized = _normalizeDOMPoint({node: t0, offset: 4})
        assert.strictEqual(normalized.node, t0)
    })

    it("normalize forward", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 0})
        assert.equal(normalized.node.data, "Example: ")
    })

    it("normalize forward -- nested", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let i = p.childNodes[1]
        let normalized = _normalizeDOMPoint({node: i, offset: 0})
        assert.equal(normalized.node.data, "italic")
    })

    it("normalize forward -- ignore comments", () => {
        let root = sandbox(
            `<p><!-- comment --->Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 0})
        assert.equal(normalized.node.data, "Example: ")
    })

    it("normalize backward", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 3})
        assert.equal(normalized.node.data, " and ")
    })

    it("normalize backward -- nested", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b></p>`
        )
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 4})
        assert.equal(normalized.node.data, "bold")
    })

    it("normalize backward -- ignore comments", () => {
        let root = sandbox(
            `<p>Example: <i>italic</i> and <b>bold</b><!-- comment ---></p>`
        )
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 5})
        assert.equal(normalized.node.data, "bold")
    })

    it("handle empty paragraph", () => {
        let root = sandbox(`<p><br></p>`)
        let p = root.childNodes[0]
        let normalized = _normalizeDOMPoint({node: p, offset: 0})
        assert.isNull(normalized)
    })
})
