package com.ruin.lsp.util

import com.intellij.openapi.roots.ExternalProjectSystemRegistry
import java.io.File
import java.nio.file.Files
import javax.xml.parsers.DocumentBuilderFactory

fun ideaFolderFile(imlPath: File, suffix: String) =
    File(imlPath.parent.plus("/.idea/").plus(suffix))


fun isRootProject(imlFile: File): Boolean {
    return if (hasMavenModule(imlFile)) {
        isRootMavenProject(imlFile)
    } else {
        true
    }
}

fun parseXml(file: File): org.w3c.dom.Document? {
    if(!Files.exists(file.toPath())) {
        return null
    }

    val documentBuilderFactory = DocumentBuilderFactory
        .newInstance()
    val documentBuilder = documentBuilderFactory.newDocumentBuilder()
    return documentBuilder.parse(file)
}

fun hasMavenModule(imlFile: File): Boolean {
    val document = parseXml(imlFile) ?: return false
    val modules = document.getElementsByTagName("module")

    return (0 until modules.length)
        .mapNotNull { modules.item(it).attributes.getNamedItem(ExternalProjectSystemRegistry.IS_MAVEN_MODULE_KEY) }
        .any { it.nodeValue == "true" }
}

fun isRootMavenProject(imlFile: File): Boolean {
    val miscXml = ideaFolderFile(imlFile, "misc.xml")
    val document = parseXml(miscXml) ?: return false
    val modules = document.getElementsByTagName("component")

    return (0 until modules.length)
        .mapNotNull { modules.item(it).attributes.getNamedItem("name") }
        .any { it.nodeValue == "MavenProjectsManager" }
}
