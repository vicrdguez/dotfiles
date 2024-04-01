local ls = require("luasnip")
local s = ls.s
local fmt = require("luasnip.extras.fmt").fmt
local i = ls.insert_node
local t = ls.text_node
local f = ls.function_node
-- local rep = require("luasnip.extras").rep

local function rep_last(index)
    return f(function(text)
        local parts = vim.split(text[1][1], ".", { plain = true })
        return parts[#parts] or ""
    end, { index }, {})
end

ls.add_snippets("all", {
    s({ trig = "ct", desc = "Resolves current time" },
        f(function()
            return os.date("%H:%M")
        end)),

    s({ trig = "cd", desc = "Resolves current date" },
        f(function()
            return os.date("%Y-%m-%d")
        end))
})

ls.add_snippets("lua", {
    s("req", fmt('local {} = require("{}")', { rep_last(1), i(1, "package") }))
})

ls.add_snippets("markdown", {
    s("<s",
        fmt([[
    ```{}
    {}
    ```
    ]], { i(1, "lang"), i(2) })),

    s("<n",
        fmt([[
    > [!NOTE]
    > {}
    ]], { i(1) })),

    s("<w",
        fmt([[
    > [!WARNING]
    > {}
    ]], { i(1) })),

    s("<i",
        fmt([[
    > [!IMPORTANT]
    > {}
    ]], { i(1) })),

    s("tt",
        fmt([[
        * [ ] {}
    ]], { i(1) })),
})


ls.add_snippets("java", {
    s("prm",
        fmt([[
        private {}({}) {{
            {}
        }}

    ]], { i(1), i(2), i(3)})),

    s("pubm",
       fmt([[
        public {}({}) {{
            {}
        }}
    ]], {i(1), i(2), i(3)})),
})

ls.add_snippets("yaml", {
    s("kzk",
        fmt([[
    ---
    apiVersion: platform.confluent.io/v1beta1
    kind: Zookeeper
    metadata:
      name: zookeeper
      namespace: confluent
    spec:
      replicas: 3
      image:
        application: confluentinc/cp-zookeeper:{}
        init: confluentinc/confluent-init-container:{}
      dataVolumeCapacity: 10Gi
      logVolumeCapacity: 10Gi
    ]], { i(1), i(2) })
    ),
    s("kbk",
        fmt([[
        ---
        apiVersion: platform.confluent.io/v1beta1
        kind: Kafka
        metadata:
          name: kafka
          namespace: confluent
        spec:
          replicas: 3
          image:
            application: confluentinc/cp-server:{}
            init: confluentinc/confluent-init-container:{}
          dataVolumeCapacity: 100Gi
          metricReporter:
            enabled: true
          dependencies:
            zookeeper:
              endpoint: zookeeper.confluent.svc.cluster.local:2181
    ]], { i(1), i(2) })
    )
})
