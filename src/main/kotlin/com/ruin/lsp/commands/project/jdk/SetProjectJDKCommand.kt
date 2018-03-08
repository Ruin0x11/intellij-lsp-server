package com.ruin.lsp.commands.project.jdk

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.externalSystem.service.execution.ExternalSystemJdkUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.*
import com.intellij.openapi.projectRoots.impl.ProjectJdkImpl
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import com.intellij.openapi.roots.OrderRootType
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.roots.ui.configuration.ProjectStructureConfigurable
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.JdkKind
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.model.SetProjectJDKResult
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.normalizeUri
import com.ruin.lsp.util.uriToPath
import org.jetbrains.idea.devkit.projectRoots.IdeaJdk
import org.jetbrains.idea.devkit.projectRoots.Sandbox


class SetProjectJDKCommand(val rootUri: String, val kind: JdkKind) : ProjectCommand<SetProjectJDKResult> {
    override fun execute(project: Project): SetProjectJDKResult {
        val rootUri = normalizeUri(rootUri)
        val jdkPath = uriToPath(rootUri)

        if (!ExternalSystemJdkUtil.isValidJdk(jdkPath)) {
            throw LanguageServerException("Path did not contain a valid JDK.")
        }

        var jdk = findExistingJdk(rootUri, kind)

        if (jdk == null) {
            val sdkType = kind.toSdkType()
            jdk = createAndAddSDK(project, rootUri, sdkType)
        }

        var success = false
        if (jdk != null) {
            SdkConfigurationUtil.setDirectoryProjectSdk(project, jdk)
            success = true
        }

        val versionString = ProjectRootManager.getInstance(project).projectSdk?.versionString
        return SetProjectJDKResult(success, versionString)
    }
}

fun JdkKind.toSdkType() =
    when(this) {
        JdkKind.INTELLIJ -> IdeaJdk.getInstance()
        JdkKind.JDK -> JavaSdk.getInstance()
    }

fun jdkHasPath(sdk: Sdk, rootUri: String) =
    if(sdk.homeDirectory != null)
        getURIForFile(sdk.homeDirectory!!).equals(rootUri, true) && sdk.sdkType == JavaSdk.getInstance()
    else
        false

fun intellijJdkHasPath(sdk: Sdk, rootUri: String): Boolean {
    if (sdk.sdkType != IdeaJdk.getInstance()) {
        return false
    }

    val data = sdk.sdkAdditionalData as? Sandbox ?: return false
    val innerJdk = data.javaSdk ?: return false

    return jdkHasPath(innerJdk, rootUri)
}


fun findExistingJdk(rootUri: String, jdkKind: JdkKind): Sdk? {
    val pred: (Sdk) -> Boolean = when(jdkKind) {
        JdkKind.JDK -> {
            { sdk -> jdkHasPath(sdk, rootUri) }
        }
        JdkKind.INTELLIJ -> {
            { sdk -> intellijJdkHasPath(sdk, rootUri) }
        }
    }

    val table = ProjectJdkTable.getInstance()
    return table.allJdks.find(pred)
}

fun findOrCreateJdk(rootUri: String): Sdk? {
    var jdk = findExistingJdk(rootUri, JdkKind.JDK)

    if (jdk == null) {
        val jdkPath = uriToPath(rootUri)
        jdk = SdkConfigurationUtil.createAndAddSDK(jdkPath, JavaSdk.getInstance())
    }

    return jdk
}

fun createAndAddSDK(project: Project, rootUri: String, sdkType: SdkType): Sdk? {
    val jdkPath = uriToPath(rootUri)

    return when(sdkType) {
        IdeaJdk.getInstance() -> {
            val innerJdk = findOrCreateJdk(rootUri)
            if (innerJdk != null) {
                val version = JavaSdk.getInstance().getVersion(innerJdk)
                if(version?.isAtLeast(JavaSdkVersion.JDK_1_8) == true) {
                    val jdk = createIdeaJdk(project, innerJdk) ?: return null
                    SdkConfigurationUtil.addSdk(jdk)
                    jdk
                } else {
                    null
                }
            } else {
                null
            }
        }
        JavaSdk.getInstance() -> {
            SdkConfigurationUtil.createAndAddSDK(jdkPath, JavaSdk.getInstance())
        }
        else -> null
    }
}

fun createIdeaJdk(project: Project, innerJdk: Sdk): Sdk? {
    val ideaJdk = ProjectJdkTable.getInstance().createSdk("IntelliJ IDEA", IdeaJdk.getInstance())
    val homePath = IdeaJdk().suggestHomePath() ?: return null

    var modificator = ideaJdk.sdkModificator
    modificator.homePath = homePath
    modificator.versionString = innerJdk.versionString
    ApplicationManager.getApplication().runWriteAction { modificator.commitChanges() }

    modificator = ideaJdk.sdkModificator
    setupIdeaInnerJdk(innerJdk, modificator)
    setupSdkRoots(project, ideaJdk, modificator, innerJdk)
    setInternalJdk(ideaJdk, modificator, innerJdk)
    ApplicationManager.getApplication().runWriteAction { modificator.commitChanges() }

    (ideaJdk as ProjectJdkImpl).resetVersionString()
    return ideaJdk
}

fun setInternalJdk(ideaJdk: Sdk, modificator: SdkModificator, innerJdk: Sdk) {
    val method = IdeaJdk::class.java.getDeclaredMethod("setInternalJdk",
        Sdk::class.java, SdkModificator::class.java, Sdk::class.java)
    method.isAccessible = true
    method.invoke(null, ideaJdk, modificator, innerJdk)
    method.isAccessible = false
}

fun setupSdkRoots(project: Project, ideaJdk: Sdk, modificator: SdkModificator, innerJdk: Sdk) {
    val projectJdksModel = ProjectStructureConfigurable.getInstance(project).projectJdksModel
    if (!projectJdksModel.isInitialized) {
        projectJdksModel.reset(project)
    }

    projectJdksModel.addSdk(innerJdk)

    val method = IdeaJdk::class.java.getDeclaredMethod("setupSdkPaths",
        Sdk::class.java, SdkModificator::class.java, SdkModel::class.java)
    method.isAccessible = true
    method.invoke(null, ideaJdk, modificator, projectJdksModel)
    method.isAccessible = false
}

fun setupIdeaInnerJdk(javaJdk: Sdk, sdkModificator: SdkModificator) {
    for (type in OrderRootType.getAllTypes()) {
        if (!(javaJdk.sdkType as SdkType).isRootTypeApplicable(type)) {
            continue
        }
        val internalRoots = javaJdk.rootProvider.getFiles(type)
        val configuredRoots = sdkModificator.getRoots(type)
        internalRoots
            .filterNot { configuredRoots.contains(it) }
            .forEach { sdkModificator.addRoot(it, type) }
    }
}
