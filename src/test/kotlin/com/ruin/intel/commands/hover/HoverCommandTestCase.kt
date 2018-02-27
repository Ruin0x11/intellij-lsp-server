package com.ruin.intel.commands.hover

class HoverCommandTestCase : HoverCommandTestBase() {
    fun `test hover gets element info`() = checkHoverContains(12, 30, "boring")
}
