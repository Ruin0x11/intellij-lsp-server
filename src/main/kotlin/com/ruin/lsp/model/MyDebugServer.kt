package com.ruin.lsp.model

import com.intellij.debugger.DebuggerInvocationUtil
import com.intellij.debugger.DebuggerManagerEx
import com.intellij.debugger.DefaultDebugEnvironment
import com.intellij.debugger.SourcePosition
import com.intellij.debugger.engine.DebugProcess
import com.intellij.debugger.engine.DebugProcessListener
import com.intellij.debugger.engine.JavaDebugProcess
import com.intellij.debugger.engine.SuspendContext
import com.intellij.debugger.impl.DebuggerManagerImpl
import com.intellij.debugger.impl.DebuggerSession
import com.intellij.debugger.impl.GenericDebuggerRunnerSettings
import com.intellij.debugger.impl.descriptors.data.StackFrameData
import com.intellij.debugger.jdi.StackFrameProxyImpl
import com.intellij.debugger.settings.DebuggerSettings
import com.intellij.execution.ExecutionException
import com.intellij.execution.Executor
import com.intellij.execution.configurations.*
import com.intellij.execution.executors.DefaultDebugExecutor
import com.intellij.execution.process.ProcessAdapter
import com.intellij.execution.process.ProcessEvent
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.runners.ExecutionEnvironmentBuilder
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.Module
import com.intellij.openapi.module.ModuleManager
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.DumbService
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.InvalidDataException
import com.intellij.openapi.util.Key
import com.intellij.openapi.util.WriteExternalException
import com.intellij.psi.JavaPsiFacade
import com.intellij.psi.PsiDocumentManager
import com.intellij.psi.PsiFile
import com.intellij.psi.search.GlobalSearchScope
import com.intellij.xdebugger.*
import com.ruin.lsp.util.asWriteAction
import com.ruin.lsp.util.getURIForFile
import com.ruin.lsp.util.resolvePsiFromUri
import org.eclipse.lsp4j.debug.*
import org.eclipse.lsp4j.debug.services.IDebugProtocolClient
import org.eclipse.lsp4j.debug.services.IDebugProtocolServer
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.jsonrpc.validation.NonNull
import org.jdom.Element
import org.junit.Assert
import java.io.File
import java.util.concurrent.CompletableFuture
import javax.swing.Icon

interface DebugClientAware {
    fun connect(client: IDebugProtocolClient)
}

data class DebugContext(var capabilities: InitializeRequestArguments? = null,
                        var project: Project? = null,
                        var session: DebuggerSession? = null)

private val LOG = Logger.getInstance(MyDebugServer::class.java)

fun SourcePosition.toSource(): Source {
    val name = this.file.name
    val path = getURIForFile(this.file)

    return Source().apply {
        this.name = name
        this.path = path
        this.sourceReference = 0
        this.presentationHint = SourcePresentationHint.NORMAL
    }
}


class MyDebugServer : IDebugProtocolServer, DebugClientAware {
    var context = DebugContext()
    var client: IDebugProtocolClient? = null

    override fun connect(client: IDebugProtocolClient) {
        this.client = client
    }

    override fun initialize(p0: InitializeRequestArguments?): CompletableFuture<Capabilities> {

        return CompletableFuture.supplyAsync {
            ApplicationManager.getApplication().invokeAndWait(
                asWriteAction(Runnable {
                    context.capabilities = p0
                    context.project = resolvePsiFromUri(getURIForFile(File("E:\\build\\doods\\src\\com\\company\\Main.java")))!!.first
                })
            )
            DumbService.getInstance(context.project!!).waitForSmartMode()
            val module = ModuleManager.getInstance(context.project!!).modules.find { it.isLoaded }!!
            val params = createJavaParameters("com.company.Main", module)
            context.session = createLocalSession(context.project!!, params)
            LOG.info("Session for ${module.name} loaded.")
            LOG.info("Debug server was initialized.")

            defaultDebugServerCapabilities()
        }
    }

    override fun loadedSources(p0: LoadedSourcesArguments?): CompletableFuture<LoadedSourcesResponse> =
        CompletableFuture.supplyAsync {
            val pos = context.session?.process?.debuggerContext?.sourcePosition
            if (pos != null) {
                LoadedSourcesResponse().apply {
                    sources = arrayOf(pos.toSource())
                }
            } else {
                LoadedSourcesResponse()
            }
        }

    override fun restartFrame(p0: RestartFrameArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun setExceptionBreakpoints(p0: SetExceptionBreakpointsArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun stepBack(p0: StepBackArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun stackTrace(p0: StackTraceArguments?): CompletableFuture<StackTraceResponse> =
    CompletableFuture.supplyAsync {
        val process = context.session!!.process ?: return@supplyAsync StackTraceResponse()
        val suspendContext = process.debuggerContext.suspendContext
        if (suspendContext != null) {
            val thread = suspendContext.thread
            val frames = thread?.frames()?.map { it.toStackFrame(context.project!!) }?.toTypedArray() ?: arrayOf()
            return@supplyAsync StackTraceResponse().apply {
                this.stackFrames = frames
                this.totalFrames = frames.size.toLong()
            }
        }

        StackTraceResponse()
    }

    override fun next(p0: NextArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun setVariable(p0: SetVariableArguments?): CompletableFuture<SetVariableResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun gotoTargets(p0: GotoTargetsArguments?): CompletableFuture<GotoTargetsResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun runInTerminal(p0: RunInTerminalRequestArguments?): CompletableFuture<RunInTerminalResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun setBreakpoints(p0: SetBreakpointsArguments): CompletableFuture<SetBreakpointsResponse> =
        CompletableFuture.supplyAsync {
            val (project, file) = resolvePsiFromUri(p0.source.path) ?: return@supplyAsync SetBreakpointsResponse()
            Assert.assertEquals(project, context.project)

            p0.breakpoints.forEach {
                setBreakpoint(context.project!!, file, it.line)
            }

            SetBreakpointsResponse()
        }

    override fun exceptionInfo(p0: ExceptionInfoArguments?): CompletableFuture<ExceptionInfoResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun reverseContinue(p0: ReverseContinueArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun completions(p0: CompletionsArguments?): CompletableFuture<CompletionsResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun threads(): CompletableFuture<ThreadsResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun modules(p0: ModulesArguments?): CompletableFuture<ModulesResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun attach(p0: MutableMap<String, Any>?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun launch(p0: MutableMap<String, Any>?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun pause(p0: PauseArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun configurationDone(p0: ConfigurationDoneArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun setFunctionBreakpoints(p0: SetFunctionBreakpointsArguments?): CompletableFuture<SetFunctionBreakpointsResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun disconnect(p0: DisconnectArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun scopes(p0: ScopesArguments?): CompletableFuture<ScopesResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun stepIn(p0: StepInArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun stepInTargets(p0: StepInTargetsArguments?): CompletableFuture<StepInTargetsResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun continue_(p0: ContinueArguments?): CompletableFuture<ContinueResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun goto_(p0: GotoArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun variables(p0: VariablesArguments?): CompletableFuture<VariablesResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun evaluate(p0: EvaluateArguments?): CompletableFuture<EvaluateResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun restart(p0: RestartArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun source(p0: SourceArguments?): CompletableFuture<SourceResponse> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }

    override fun stepOut(p0: StepOutArguments?): CompletableFuture<Void> {
        TODO("not implemented") //To change body of created functions use File | Settings | File Templates.
    }
}

fun StackFrameProxyImpl.toStackFrame(project: Project): StackFrame {
    val data = StackFrameData(this).createDescriptor(project)
    val sourcePath = this.location().sourcePath()
    val sourceName = this.location().sourceName()
    val source: Source? = Source().apply {
        path = sourcePath
        name = sourceName
    }
    val id: Long? = this.hashCode().toLong()
    val name: String? = data.name
    val line: Long? = this.location().lineNumber().toLong()
    return StackFrame().apply {
        this.source = source
        this.id = id
        this.name = name
        this.line = line
        this.column = 0
    }
}

fun createJavaParameters(mainClass: String, module: Module): JavaParameters {
    val parameters = JavaParameters()
    parameters.configureByModule(module, JavaParameters.JDK_AND_CLASSES_AND_TESTS)
    parameters.mainClass = mainClass
    return parameters
}

fun setBreakpoint(myProject: Project, myFile: PsiFile, line: Long) =
    DebuggerInvocationUtil.invokeAndWait(myProject, {
        val breakpointManager = DebuggerManagerEx.getInstanceEx(myProject).breakpointManager
        val document = PsiDocumentManager.getInstance(myProject).getDocument(myFile)
        breakpointManager.addLineBreakpoint(document, line.toInt())
    }, ApplicationManager.getApplication().defaultModalityState)

@Throws(ExecutionException::class, InterruptedException::class)
fun createLocalSession(myProject: Project, javaParameters: JavaParameters): DebuggerSession {
    //createBreakpoints(myProject, javaParameters.mainClass)
    val psiClass = JavaPsiFacade.getInstance(myProject).findClass("com.company.Main", GlobalSearchScope.allScope(myProject))!!
    setBreakpoint(myProject, psiClass.containingFile, 5)

    DebuggerSettings.getInstance().DEBUGGER_TRANSPORT = DebuggerSettings.SOCKET_TRANSPORT

    val debuggerRunnerSettings = GenericDebuggerRunnerSettings()
    debuggerRunnerSettings.LOCAL = true

    val debugParameters = DebuggerManagerImpl.createDebugParameters(javaParameters, debuggerRunnerSettings, false)

    val environment = ExecutionEnvironmentBuilder(myProject, DefaultDebugExecutor.getDebugExecutorInstance())
        .runnerSettings(debuggerRunnerSettings)
        .runProfile(MockConfiguration())
        .build()
    val javaCommandLineState = object : JavaCommandLineState(environment) {
        override fun createJavaParameters(): JavaParameters {
            return javaParameters
        }

        @Throws(ExecutionException::class)
        override fun createCommandLine(): GeneralCommandLine {
            return getJavaParameters().toCommandLine()
        }
    }

    var myDebuggerSession: DebuggerSession? = null

    ApplicationManager.getApplication().invokeAndWait {
        try {
            myDebuggerSession = DebuggerManagerEx.getInstanceEx(myProject)
                .attachVirtualMachine(DefaultDebugEnvironment(ExecutionEnvironmentBuilder(myProject, DefaultDebugExecutor.getDebugExecutorInstance())
                    .runProfile(MockConfiguration())
                    .build(), javaCommandLineState, debugParameters, false))
            XDebuggerManager.getInstance(myProject).startSession(javaCommandLineState.environment, object : XDebugProcessStarter() {
                override fun start(session: XDebugSession): XDebugProcess {
                    return JavaDebugProcess.create(session, myDebuggerSession)
                }
            })
        } catch (e: ExecutionException) {
            LOG.error(e)
        }
    }
    val myDebugProcess = myDebuggerSession?.process

    myDebugProcess?.addProcessListener(object : ProcessAdapter() {
        override fun onTextAvailable(event: ProcessEvent, outputType: Key<*>) {
            LOG.info("$outputType ${event.text}")
        }
    })

    return myDebuggerSession!!
}

class MockConfiguration : ModuleRunConfiguration {
    override fun getModules(): Array<Module> {
        return Module.EMPTY_ARRAY
    }

    override fun getIcon(): Icon? {
        return null
    }

    override fun getFactory(): ConfigurationFactory {
        return UnknownConfigurationType.FACTORY
    }

    override fun setName(name: String) {}

    override fun getConfigurationEditor(): SettingsEditor<out RunConfiguration> {
        throw UnsupportedOperationException()
    }

    override fun getProject(): Project? {
        return null
    }

    override fun clone(): RunConfiguration {
        return this
    }

    @Throws(ExecutionException::class)
    override fun getState(executor: Executor, env: ExecutionEnvironment): RunProfileState? {
        return null
    }

    override fun getName(): String {
        return ""
    }

    @Throws(InvalidDataException::class)
    override fun readExternal(element: Element?) {
    }

    @Throws(WriteExternalException::class)
    override fun writeExternal(element: Element?) {
    }
}

fun defaultDebugServerCapabilities() =
    Capabilities().apply {
        supportsConfigurationDoneRequest = false
        supportsFunctionBreakpoints = false
        supportsConditionalBreakpoints = false
        supportsHitConditionalBreakpoints = false
        supportsEvaluateForHovers = false
        exceptionBreakpointFilters = null
        supportsStepBack = false
        supportsSetVariable = false
        supportsRestartFrame = false
        supportsGotoTargetsRequest = false
        supportsStepInTargetsRequest = false
        supportsCompletionsRequest = false
        supportsModulesRequest = false
        additionalModuleColumns = null
        supportedChecksumAlgorithms = null
        supportsRestartRequest = false
        supportsExceptionOptions = false
        supportsValueFormattingOptions = false
        supportsExceptionInfoRequest = false
        supportTerminateDebuggee = false
        supportsDelayedStackTraceLoading = false
        supportsLoadedSourcesRequest = false
    }

class MyDebugListener : DebugProcessListener {
    override fun threadStopped(proc: DebugProcess?, thread: ThreadReference?) {
        super.threadStopped(proc, thread)
    }

    override fun processDetached(process: DebugProcess?, closedByUser: Boolean) {
        super.processDetached(process, closedByUser)
    }

    override fun paused(suspendContext: SuspendContext?) {
        super.paused(suspendContext)
    }

    override fun resumed(suspendContext: SuspendContext?) {
        super.resumed(suspendContext)
    }

    override fun attachException(state: RunProfileState?, exception: ExecutionException?, remoteConnection: RemoteConnection?) {
        super.attachException(state, exception, remoteConnection)
    }

    override fun connectorIsReady() {
        super.connectorIsReady()
    }

    override fun processAttached(process: DebugProcess?) {
        super.processAttached(process)
    }

    override fun threadStarted(proc: DebugProcess?, thread: ThreadReference?) {
        super.threadStarted(proc, thread)
    }
}
