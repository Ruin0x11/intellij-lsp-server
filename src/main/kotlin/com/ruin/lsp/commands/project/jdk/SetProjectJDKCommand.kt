package com.ruin.lsp.commands.project.jdk

import com.intellij.openapi.project.Project
import com.intellij.openapi.projectRoots.ProjectJdkTable
import com.intellij.openapi.projectRoots.SdkType
import com.intellij.openapi.projectRoots.SdkTypeId
import com.intellij.openapi.projectRoots.impl.SdkConfigurationUtil
import com.ruin.lsp.commands.ProjectCommand
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.uriToPath

class SetProjectJDKCommand(val rootUri: String) : ProjectCommand<Boolean> {
    override fun execute(project: Project): Boolean {
        //val compilerConfig = CompilerConfiguration.getInstance(project)

        val type = SdkType.getAllTypes().first()

        val table = ProjectJdkTable.getInstance()
        var jdk = table.allJdks.find { sdk ->
            if(sdk.homeDirectory != null)
                getURIForFile(sdk.homeDirectory!!) == rootUri && sdk.sdkType == type
            else
                false
        }

        if (jdk == null) {
            jdk = SdkConfigurationUtil.createAndAddSDK(uriToPath(rootUri), type)
        }

        if (jdk != null) {
            if (jdk.sdkType == type) {
                SdkConfigurationUtil.setDirectoryProjectSdk(project, jdk)
                return true
            }
        }

        return false
    }
}
