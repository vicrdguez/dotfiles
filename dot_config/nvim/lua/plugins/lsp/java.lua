local home = os.getenv("HOME")
local java_install_path = home .. "/.asdf/installs/java/"
local utils = require("utils")
local map = utils.map
local nmap = utils.nmap

local M = {}

local jdtls_opts = {
    cmd = {},
    capabilities = {},
    on_attach = {},
    root_dir = {},
    -- init_options = {},
    -- https://github.com/eclipse/eclipse.jdt.ls/wiki/Running-the-JAVA-LS-server-from-the-command-line#initialize-request
    settings = {
        java = {
            signatureHelp = { enabled = true },
            referencesCodeLens = { enabled = true },
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
            import = {
                gradle = {
                    enabled = true,
                    wrapper = { enabled = true },
                },
                maven = { enabled = true },
            },
            configuration = {
                runtimes = {
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
                },
            },
        },
    },
}

M.get_opts = function(jdtls)
    local mason_pkg_dir = vim.fn.stdpath("data") .. "/mason/packages"
    local jdtls_bin = mason_pkg_dir .. "/jdtls/bin/jdtls"
    local root_markers = { ".gradle", "gradlew", ".git", "pom.xml", "settings.gradle" }
    local root_dir = jdtls.setup.find_root(root_markers)
    local project_name = vim.fn.fnamemodify(root_dir, ":p:h:t")
    -- vim.notify("Java project name: "..project_name)
    local workspace_dir = home .. "/.cache/jdtls/workspace/" .. project_name

    -- The command is not necesary if using the provided python wrapper (jdtls_bin)
    -- Keeping it here for reference of what is being configured
    -- jdtls_opts.cmd = {
    --     home.."~/.asdf/installs/java/temurin-17.0.6+10",
    --     "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    --     "-Dosgi.bundles.defaultStartLevel=4",
    --     "-Declipse.product=org.eclipse.jdt.ls.core.product",
    --     "-Dlog.protocol=true",
    --     "-Dlog.level=ALL",
    --     "-Xmx4g",
    --     "--add-modules=ALL-SYSTEM",
    --     "--add-opens", "java.base/java.util=ALL-UNNAMED",
    --     "--add-opens", "java.base/java.lang=ALL-UNNAMED",
    --     "-jar", vim.fn.glob(home .. "/mason/packages/jdtls/plugins/org.eclipse.equinox.launcher_*.jar"),
    --     "-configuration", home .. "/dev/eclipse/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/config_linux",
    --     "-data", workspace_dir,
    --     -- "-data",
    --     -- workspace_dir
    -- }

    local capabilities = vim.lsp.protocol.make_client_capabilities()
    capabilities.workspace.configuration = true
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
    -- capabilities.document_formatting = false

    jdtls_opts.capabilities = capabilities

    jdtls_opts.root_dir = root_dir

    local extended_capabilities = jdtls.extendedClientCapabilities
    extended_capabilities.resolveAdditionalTextEditsSupport = true

    local bundles = {
        vim.fn.glob(
            mason_pkg_dir .. "/java-debug-adapter/extension/server/com.microsoft.java.debug.plugin-*.jar",
            true
        ),
    }

    ---@diagnostic disable-next-line: missing-parameter
    vim.list_extend(
        bundles,
        ---@diagnostic disable-next-line: missing-parameter
        vim.split(vim.fn.glob(mason_pkg_dir .. "/java-test/extension/server/*.jar", true), "\n")
    )

    jdtls_opts.init_options = {
        bundles = bundles,
        extendedClientCapabilities = extended_capabilities,
    }
    jdtls_opts.cmd = {
        jdtls_bin,
        "-configuration",
        home .. "/.cache/jdtls",
        "-data",
        workspace_dir,
    }

    jdtls_opts.on_attach = function(client, bufnr)
        jdtls.setup_dap({ hotcodereplace = "auto" })
        require("jdtls.dap").setup_dap_main_class_configs()
        jdtls.setup.add_commands()

        vim.notify("Lsp client attaching: [" .. client.name .. "]")
        -- nmap("<leader>dt", jdtls.test_neareast_method, { buffer = bufnr, desc = "Test neaerest method" })
        nmap("<leader>o", jdtls.organize_imports, { buffer = bufnr, desc = "Jdtls: organize imports", silent = true })
        nmap("<leader>ev", jdtls.extract_variable, { buffer = bufnr, desc = "Extract variable", silent = true })
        nmap("<leader>ec", jdtls.extract_constant, { buffer = bufnr, desc = "Extract constant", silent = true })
        map("v", "<leader>em", function()
            require("jdtls").extract_method(true)
        end, { buffer = bufnr, desc = "Extract method", silent = true })

        vim.notify("Lsp client attached: [" .. client.name .. "]")
    end

    return jdtls_opts
end

return M
