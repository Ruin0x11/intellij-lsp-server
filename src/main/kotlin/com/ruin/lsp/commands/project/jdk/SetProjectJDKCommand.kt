package com.ruin.lsp.commands.project.jdk

import com.intellij.openapi.externalSystem.service.execution.ExternalSystemJdkUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.JavaSdk
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.SdkType
import com.intellij.openapi.projectRoots.SdkTypeId
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.model.LanguageServerException
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.uriToPath

class SetProjectJDKCommand(val rootUri: String) : ProjectCommand<Boolean> {
    override fun execute(project: Project): Boolean {
        //val compilerConfig = CompilerConfiguration.getInstance(project)

        val jdkPath = uriToPath(rootUri)

        if (!ExternalSystemJdkUtil.isValidJdk(jdkPath)) {
            throw LanguageServerException("Path did not contain a valid JDK.")
        }

        val table = ProjectJdkTable.getInstance()
        var jdk = table.allJdks.find { sdk ->
            if(sdk.homeDirectory != null)
                getURIForFile(sdk.homeDirectory!!) == rootUri && sdk.sdkType == JavaSdk.getInstance()
            else
                false
        }

        if (jdk == null) {
            jdk = SdkConfigurationUtil.createAndAddSDK(jdkPath, JavaSdk.getInstance())
        }

        if (jdk != null) {
            SdkConfigurationUtil.setDirectoryProjectSdk(project, jdk)
            return true
        }

        return false
    }
}
