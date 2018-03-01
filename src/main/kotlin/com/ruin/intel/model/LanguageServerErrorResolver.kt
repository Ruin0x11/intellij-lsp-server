package com.ruin.intel.model

import com.fasterxml.jackson.databind.JsonNode
import com.googlecode.jsonrpc4j.ErrorData
import com.googlecode.jsonrpc4j.ErrorResolver
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError.BULK_ERROR
import com.googlecode.jsonrpc4j.ErrorResolver.JsonError.ERROR_NOT_HANDLED
import java.lang.reflect.Method

class LanguageServerErrorResolver : ErrorResolver {
    override fun resolveError(t: Throwable?, method: Method?, nodes: MutableList<JsonNode>?): ErrorResolver.JsonError {
        return if (t is LanguageServerException?) {
            ErrorResolver.JsonError(BULK_ERROR.code, t?.message, ErrorData(t?.javaClass?.name, t?.message))
        } else {
            ErrorResolver.JsonError(ERROR_NOT_HANDLED.code, t?.message, ErrorData(t?.javaClass?.name, t?.message))
        }
    }
}
