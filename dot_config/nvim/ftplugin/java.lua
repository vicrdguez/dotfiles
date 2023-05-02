local jdtls = require("jdtls")
local opts = require("plugins.lsp.java").get_opts(jdtls)
jdtls.start_or_attach(opts)
