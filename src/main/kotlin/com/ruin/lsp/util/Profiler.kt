package com.ruin.lsp.util

import com.intellij.openapi.diagnostic.Logger
import org.eclipse.lsp4j.services.LanguageClient
import java.util.ArrayList
import java.util.concurrent.TimeUnit
import kotlin.collections.HashMap
import kotlin.collections.forEach

/**
 * Utility class for profiling method calls
 *
 * @author dhleong
 */
open class Profiler(private var context: LanguageClient?) {
    private val LOG = Logger.getInstance(Profiler::class.java)
    private val start: Long = System.nanoTime()
    private val intervals = ArrayList<Pair<String, Long>>()

    private var last: Long = 0

    init {
        last = start
    }

    open fun mark(label: String) {
        if (!ENABLED) {
            // don't bother
            return
        }

        val now = System.nanoTime()
        val delta = now - last
        last = now

        intervals.add(Pair(label, delta))
    }

    open fun finish(label: String) {
        sActiveProfilers.remove(context)

        if (!ENABLED) {
            return
        }

        mark(label)
        val total = System.nanoTime() - start
        intervals.forEach { (label, duration) ->
            LOG.debug(format(duration, label))
        }
        LOG.debug(format(total, "Total"))
    }

    open fun switchContext(newContext: LanguageClient) {
        sActiveProfilers.remove(context)
        sActiveProfilers.put(newContext, this)
        context = newContext
    }
}

private val ENABLED = true

val DUMMY = object : Profiler(null) {
    override fun mark(label: String) {
        // nop
    }

    override fun finish(label: String) {
        // nop
    }

    override fun switchContext(newContext: LanguageClient) {
        // nop
    }
}

private val sActiveProfilers = HashMap<LanguageClient?, Profiler>()

private fun format(duration: Long, label: String?): String {
    val millis = TimeUnit.MILLISECONDS.convert(duration, TimeUnit.NANOSECONDS)
    return if (label == null) {
        String.format("[%6d] TOTAL", millis)
    } else {
        String.format("[+%5d] %s", millis, label)
    }
}

fun startProfiler(context: LanguageClient): Profiler {
    val newProfiler = Profiler(context)
    sActiveProfilers.put(context, newProfiler)
    return newProfiler
}

/**
 * Retrieve an existing Profiler from its context. This
 * will never return null, but if profiling is disabled
 * or if you never actually started any profiling, this
 * will just return a dummy.
 *
 * For a production class I would be skeptical of this decision,
 * but this class is only really for debugging, has a
 * central point of fail, and was designed to "stay out
 * of the way," so I think this works
 *
 * @param context The context passed to #start(), or
 * subsequently set with #switchContext()
 */
fun with(context: LanguageClient): Profiler {
    val existing = sActiveProfilers[context]
    if (existing != null) {
        return existing
    } else if (ENABLED && !sActiveProfilers.isEmpty()) {
        throw IllegalArgumentException("No profiler for context " + context)
    }

    // either we're disabled or there are no active profilers at all.
    //  just give them a dummy so they don't crash
    return DUMMY
}
