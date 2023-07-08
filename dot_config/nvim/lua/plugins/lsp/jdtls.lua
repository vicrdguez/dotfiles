-- jdtls config
--
--
local custom = require("custom.utils")
local map = custom.map
local nmap = custom.nmap
local augroup = custom.augroup


local M = {}
local cache_vars = {}
local root_markers = { ".gradle", "gradlew", ".git", "pom.xml" }
local mason_registry = require("mason-registry")

local home = os.getenv("HOME")
-- using rtx to multi-verion java
local java_install_path = home .. ".local/share/rtx/installs/java/"
--/Users/vrodriguez/.local/share/rtx/installs/java/temurin-17.0.6+10/bin/java

local function get_jdtls_paths(jdtls)
    if cache_vars.paths then
        return cache_vars.paths
    end

    local path = {}

    path.root_dir = jdtls.setup.find_root(root_markers)
    local project_name = vim.fn.fnamemodify(path.root_dir, ":p:h:t")
    path.workspace_dir = vim.fn.stdpath("cache") .. "jdtls/workspace/" .. project_name
    -- We used the python wrapper to avoid to set up the whole java command
    -- path.jdtls_bin = mason_registry.get_package("jdtls"):get_install_path() .. "bin/jdtls"
    local jdtls_install_dir = mason_registry.get_package("jdtls"):get_install_path()

    path.jdtls_bin = jdtls_install_dir .. "/bin/jdtls"
    path.launcher_jar = vim.fn.glob(jdtls_install_dir .. '/plugins/org.eclipse.equinox.launcher_*.jar')

    if vim.fn.has('mac') == 1 then
        path.platform_config = jdtls_install_dir .. '/config_mac'
    elseif vim.fn.has('unix') == 1 then
        path.platform_config = jdtls_install_dir .. '/config_linux'
    elseif vim.fn.has('win32') == 1 then
        path.platform_config = jdtls_install_dir .. '/config_win'
    end

    path.bundles = {}

    local java_debug_path = mason_registry.get_package("java-debug-adapter"):get_install_path()
    local java_debug_bundle = vim.split(
        vim.fn.glob(java_debug_path .. "/extension/server/com.microsoft.java.debug.plugin-*.jar"),
        "\n"
    )
    local vscode_java_test = mason_registry.get_package("java-test"):get_install_path()
    local vscode_java_bundle = vim.split(vim.fn.glob(vscode_java_test .. "/extension/server/*.jar", true), "\n")

    -- add jars to the bundle list if there are any
    if java_debug_bundle[1] ~= "" then
        vim.list_extend(path.bundles, java_debug_bundle)
    end

    if vscode_java_bundle[1] ~= "" then
        vim.list_extend(path.bundles, vscode_java_bundle)
    end

    path.runtimes = {
        {
            name = "JavaSE-1.8",
            path = java_install_path .. "temurin-8.0.362+9",
        },
        {
            name = "JavaSE-11",
            path = java_install_path .. "temurin-11.0.18+10",
        },
        {
            name = "JavaSE-17",
            path = java_install_path .. "temurin-17.0.6+10",
            default = true,
        },
    }

    cache_vars.paths = path

    return path
end

local function get_jdtls_capabilities(jdtls)
    if cache_vars.capabilities and cache_vars.capabilities then
        return cache_vars.capabilities, cache_vars.extended_capabilities
    end

    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.workspace.configuration = true
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    local extended_capabilities = jdtls.extendedClientCapabilities
    extended_capabilities.resolveAdditionalTextEditsSupport = true

    local ok_cmp, cmp_lsp = pcall(require, "cmp_nvim_lsp")
    capabilities = vim.tbl_deep_extend(
        "force",
        capabilities,
        ok_cmp and cmp_lsp.default_capabilities() or {}
    )
    cache_vars.extended_capabilities = extended_capabilities
    cache_vars.capabilities = capabilities

    return capabilities, extended_capabilities
end

local function enable_codelens(bufnr)
    pcall(vim.lsp.codelens.refresh)

    vim.api.nvim_create_autocmd("BufWritePost", {
        buffer = bufnr,
        group = augroup("jdtls"),
        desc = "Refresh codelens",
        callback = function()
            pcall(vim.lsp.codelens.refresh)
        end
    })
end

local function enable_debugger(bufnr)
    vim.notify("ENABLE DEBUGGER")
    require("jdtls").setup_dap({ hotcodereplace = "auto" })
    require("jdtls.dap").setup_dap_main_class_configs()
end


local function jdtls_on_attach(client, bufnr)
    enable_debugger(bufnr)
    enable_codelens(bufnr)

    vim.notify("jdtls attached")

    local opts = { buffer = bufnr }
    nmap("<A-o>", "<cmd>lua require('jdtls').organize_imports()<cr>", opts)
    nmap("crv", "<cmd>lua require('jdtls').extract_variable()<cr>", opts)
    nmap("crc", "<cmd>lua require('jdtls').extract_constant()<cr>", opts)
    map("x", "crv", "<esc><cmd>lua require('jdtls').extract_variable(true)<cr>", opts)
    map("x", "crc", "<esc><cmd>lua require('jdtls').extract_constant(true)<cr>", opts)
    map("x", "crm", "<esc><Cmd>lua require('jdtls').extract_method(true)<cr>", opts)
end

M.jdtls_setup = function(event)
    local jdtls = require("jdtls")
    local paths = get_jdtls_paths(jdtls)

    local capabilities, extended_capabilities = get_jdtls_capabilities(jdtls)

    local cmd = {
        java_install_path .. "temurin-17.0.6+10/bin/java",
        "-Declipse.application=org.eclipse.jdt.ls.core.id1",
        "-Dosgi.bundles.defaultStartLevel=4",
        "-Declipse.product=org.eclipse.jdt.ls.core.product",
        "-Dlog.protocol=true",
        "-Dlog.level=ALL",
        "-Xmx4g",
        "--add-modules=ALL-SYSTEM",
        "--add-opens", "java.base/java.util=ALL-UNNAMED",
        "--add-opens", "java.base/java.lang=ALL-UNNAMED",
        "-jar", paths.launcher_jar,
        "-configuration", paths.platform_config,
        "-data", paths.workspace_dir,
        -- "-data",
        -- workspace_dir
    }


    -- for _, v in ipairs(cmd) do
    --     vim.cmd("!echo '".. v .."' > my_file.txt")
    -- end

    local lsp_settings = {
        java = {
            jdt = {
                ls = {
                    -- vmargs = "-XX:+UseParallelGC -XX:GCTimeRatio=4 -XX:AdaptiveSizePolicyWeight=90 -Dsun.zip.disableMemoryMapping=true -Xmx1G -Xms100m"
                    java = {
                        home = java_install_path .. "temurin-17.0.6+10",
                    }
                }
            },
            signatureHelp = { enabled = true },
            referencesCodeLens = { enabled = true },
            implementationCodeLens = { enabled = true },
            inlayHints = {
                parameterNames = {
                    enabled = "all" -- literals, all, none
                },
            },
            completion = {
                favoriteStaticMembers = {
                    "org.junit.Assert.*",
                    "org.junit.Assume.*",
                    "org.junit.jupiter.api.Assertions.*",
                    "org.junit.jupiter.api.Assumptions.*",
                    "org.junit.jupiter.api.DynamicContainer.*",
                    "org.junit.jupiter.api.DynamicTest.*",
                },
                filteredTypes = {
                    "org.junit.Assert.*",
                    "org.junit.Assume.*",
                    "org.junit.jupiter.api.Assertions.*",
                    "org.junit.jupiter.api.Assumptions.*",
                    "org.junit.jupiter.api.DynamicContainer.*",
                    "org.junit.jupiter.api.DynamicTest.*",
                },
            },
            sources = {
                organizeImports = {
                    startThreshold = 9999,
                    stasticStarThreshold = 9999,
                },
            },
            codeGeneration = {
                toString = {
                    template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
                },
                useBlocks = true,
                generateComments = true,
            },
            format = {
                settings = {
                    url = home .. "/.local/share/eclipse/eclipse-java-google-style.xml",
                    profile = "GoogleStyle",
                },
            },
            contentProvider = {
                preferred = "fernflower"
            },
            extendedClientCapabilities = extended_capabilities,
            import = {
                gradle = {
                    enabled = true,
                    wrapper = { enabled = true },
                },
                maven = { enabled = true },
            },
            configuration = {
                updateBuildConfiguration = "interactive",
                runtimes = paths.runtimes,
            },
        },
    }

    jdtls.start_or_attach({
        cmd = cmd,
        settings = lsp_settings,
        on_attach = jdtls_on_attach(),
        capabilities = capabilities,
        root_dir = paths.root_dir,
        flags = {
            allow_incremental_sync = true,
        },
        init_options = {
            bundles = paths.bundles,
            extendedClientCapabilities = extended_capabilities
        },
    })
end


return M
