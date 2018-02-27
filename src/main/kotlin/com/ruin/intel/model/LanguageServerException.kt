package com.ruin.intel.model

import com.googlecode.jsonrpc4j.ErrorResolver

class LanguageServerException(val error: ErrorResolver.JsonError) : Exception(error.message)