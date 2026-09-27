ApiClientContract <- R6::R6Class(
  "ApiClientContract",
  public = list(
    run = function(context) {
      private$transient_retry(context)
      private$graphql_rate_limit(context)
      private$response_errors(context)
      private$corrupt_rate_state(context)
      private$authentication(context)
      private$bounded_rate_wait(context)
      private$json_decoding(context)
      private$explicit_requests(context)
      private$retry_limits(context)
      private$graphql_validation(context)
      private$rate_pacing(context)
      private$url_encoding(context)
      invisible(TRUE)
    }
  ),
  private = list(
    graphql_validation = function(context) {
      for (payload in c("42", "null", "[]", "{}")) {
        project <- context$new_test_project()
        wire <- MockHttpTransport$new(responses = list(list(status = 200L, body_text = payload)))
        bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(), wire)
        result <- bundle$client$graphql("query { viewer { login } }")
        context$check(paste("GraphQL rejeita corpo sem objeto data/errors:", payload),
          !bundle$client$response_ok(result) && nzchar(result$error))
      }
      project <- context$new_test_project()
      wire <- MockHttpTransport$new(responses = list(list(status = 200L,
        body = list(errors = list(list(type = "RATE_LIMITED", message = "limit"))))))
      bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(), wire)
      result <- bundle$client$graphql("query { viewer { login } }")
      context$check("GraphQL reconhece RATE_LIMITED no campo type da resposta GitHub",
        !bundle$client$response_ok(result) && isTRUE(result$rate_limited) &&
          inherits(bundle$client$response_condition(result), "github_rate_limit_error"))
      context$check("condição de erro nativa preserva mensagem alternativa e classes da API",
        conditionMessage(bundle$client$response_condition(list(status = 500L, error = ""), "falha esperada")) == "falha esperada")
    },
    rate_pacing = function(context) {
      project <- context$new_test_project(list(core_interval_seconds = 0.5))
      runtime <- MockRuntime$new(1000)
      bundle <- context$new_http_client(project$config, project$store, runtime, MockHttpTransport$new("permission"))
      bundle$client$api("/repos/owner/project")
      bundle$client$api("/repos/owner/project")
      context$check("espera máxima zero para rate limit preserva o intervalo normal entre chamadas", runtime$now() == 1000.5)
      bundle$limiter$record("core", list(github_remaining = "0", github_reset = "1010"))
      state_path <- project$config$resolve(project$config$get("api", "rate_state_path"))
      before <- project$store$hash_file(state_path)
      context$check("uma espera rejeitada não reserva uma chamada futura no estado compartilhado",
        context$errors(bundle$limiter$wait_for_slot("core")) && identical(before, project$store$hash_file(state_path)))
      project <- context$new_test_project(list(max_rate_wait_seconds = 2))
      runtime <- MockRuntime$new(1000)
      wire <- MockHttpTransport$new("rate_then_success")
      bundle <- context$new_http_client(project$config, project$store, runtime, wire)
      result <- bundle$client$api("/repos/owner/project")
      context$check("orçamento de espera inclui a margem de segurança do Retry-After",
        result$status == 429L && wire$request_count() == 1L && runtime$now() == 1000)
    },
    url_encoding = function(context) {
      project <- context$new_test_project()
      wire <- MockHttpTransport$new("raw")
      bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(), wire)
      bundle$client$raw("owner/project", "sha", "docs/100%/20%20.md")
      raw_url <- wire$last_url()
      bundle$client$api("/search/repositories", list(q = "100% data&x"))
      context$check("URLs codificam percentuais literais, espaços e separadores nos valores",
        grepl("docs/100%25/20%2520.md", raw_url, fixed = TRUE) &&
          grepl("q=100%25%20data%26x", wire$last_url(), fixed = TRUE))
    },
    explicit_requests = function(context) {
      project <- context$new_test_project()
      bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(), MockHttpTransport$new("headers"))
      transport <- bundle$http_transport
      explicit <- transport$request_json("https://example.test", "core", return_headers = TRUE)
      without_headers <- transport$request_json("https://example.test", "core")
      context$check("requisições JSON explícitas preservam retorno, headers e bloco HTTP final",
        isTRUE(explicit$body$ok) && is.null(without_headers$headers) &&
          explicit$rate$github_remaining == "10" && length(explicit$headers) == 5L)
      raw_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(), MockHttpTransport$new("raw_bom"))
      explicit_text <- raw_bundle$http_transport$request_text("https://example.test", "raw")
      context$check("requisições textuais explícitas preservam o corpo e o BOM",
        explicit_text$status == 200L && identical(explicit_text$body, paste0("\ufeff", "text")))
    },
    retry_limits = function(context) {
      project <- context$new_test_project()
      wire <- MockHttpTransport$new("retry_exhausted")
      runtime <- MockRuntime$new(8000)
      bundle <- context$new_http_client(project$config, project$store, runtime, wire)
      response <- bundle$client$api("/repos/owner/project")
      context$check("erro transitório persistente respeita número máximo de tentativas",
        response$status == 503L && wire$request_count() == 2L && runtime$now() == 8001)
      wire <- MockHttpTransport$new("transport_error")
      bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(9000), wire)
      response <- bundle$http_transport$request_text("https://example.test", "raw")
      context$check("falha de transporte não transforma HTTP 200 parcial em sucesso",
        response$status == 0L && wire$request_count() == 2L && grepl("timeout", response$error, fixed = TRUE))
      project <- context$new_test_project(list(max_rate_wait_seconds = 10))
      runtime <- MockRuntime$new(10000)
      wire <- MockHttpTransport$new("rate_then_success")
      bundle <- context$new_http_client(project$config, project$store, runtime, wire)
      response <- bundle$client$api("/repos/owner/project")
      context$check("rate limit recuperável aguarda o prazo e repete a mesma operação",
        response$status == 200L && wire$request_count() == 2L && runtime$now() == 10003)
    },
    transient_retry = function(context) {
      project <- context$new_test_project()
      runtime <- MockRuntime$new(1000)
      wire <- MockHttpTransport$new("retry")
      bundle <- context$new_http_client(project$config, project$store, runtime, wire)
      response <- bundle$client$api("/repos/owner/project")
      context$check("cliente HTTP repete erro transitório usando transporte e relógio injetados",
        response$status == 200L && isTRUE(response$body$ok) && wire$request_count() == 2L && runtime$now() >= 1001)
    },
    graphql_rate_limit = function(context) {
      project <- context$new_test_project()
      graphql_runtime <- MockRuntime$new(2000)
      graphql_bundle <- context$new_http_client(project$config, project$store, graphql_runtime,
        MockHttpTransport$new("graphql"))
      graphql_response <- graphql_bundle$client$graphql("query { viewer { login } }")
      graphql_condition <- graphql_bundle$client$response_condition(graphql_response)
      rate_state_path <- project$config$resolve(project$config$get("api", "rate_state_path", "outputs/metadata/github_api_rate_state.json"))
      rate_state <- jsonlite::fromJSON(rate_state_path, simplifyVector = FALSE)
      context$check("erro GraphQL estruturado vira rate limit e registra o adiamento",
        graphql_bundle$client$is_rate_limit_error(graphql_condition) &&
          rate_state$not_before$graphql > graphql_runtime$now())
    },
    response_errors = function(context) {
      project <- context$new_test_project()
      malformed_graphql_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(2500),
        MockHttpTransport$new("malformed_graphql_errors"))
      malformed_graphql <- malformed_graphql_bundle$client$graphql("query { viewer { login } }")
      context$check("cliente rejeita errors GraphQL fora do formato sem erro de coerção",
        !malformed_graphql_bundle$client$response_ok(malformed_graphql) &&
          grepl("formato inválido", malformed_graphql$error, fixed = TRUE))
      permission_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(3000),
        MockHttpTransport$new("permission"))
      permission <- permission_bundle$client$api("/repos/owner/private")
      context$check("403 de permissão com quota disponível não é classificado como rate limit",
        permission$status == 403L && !isTRUE(permission$rate_limited))
    },
    corrupt_rate_state = function(context) {
      project <- context$new_test_project()
      state_path <- project$config$resolve(project$config$get("api", "rate_state_path", "outputs/metadata/github_api_rate_state.json"))
      dir.create(dirname(state_path), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(list(last_request = list(core = list(unexpected = "invalid"))), state_path,
        auto_unbox = TRUE, pretty = FALSE)
      corrupt_state_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(3001),
        MockHttpTransport$new("permission"))
      corrupt_state_response <- corrupt_state_bundle$client$api("/repos/owner/project")
      context$check("estado persistido de limite malformado é ignorado sem interromper a chamada",
        corrupt_state_response$status == 403L)
    },
    authentication = function(context) {
      project <- context$new_test_project()
      previous_token <- Sys.getenv("GITHUB_TOKEN", unset = NA_character_)
      on.exit(if (is.na(previous_token)) Sys.unsetenv("GITHUB_TOKEN") else Sys.setenv(GITHUB_TOKEN = previous_token), add = TRUE)
      Sys.unsetenv("GITHUB_TOKEN")
      writeLines('GITHUB_TOKEN="offline-token"', file.path(project$root, ".env"))
      token_wire <- MockHttpTransport$new("permission")
      token_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(3001), token_wire)
      token_bundle$client$assert_authenticated()
      token_bundle$client$api("/repos/owner/project")
      context$check("cliente lê token local sem manter as aspas do arquivo .env",
        identical(token_wire$last_authorization(), "Authorization: Bearer offline-token"))
      raw_wire <- MockHttpTransport$new("raw")
      raw_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(3002), raw_wire)
      raw_document <- raw_bundle$client$raw("owner/project", "commit", "README.md")
      context$check("download de arquivo público não envia token ao host raw do GitHub",
        raw_document$status == 200L && identical(raw_document$text, "public README content") &&
          identical(raw_wire$last_authorization(), ""))
    },
    bounded_rate_wait = function(context) {
      project <- context$new_test_project()
      rate_wire <- MockHttpTransport$new("rate_limit")
      rate_runtime <- MockRuntime$new(4000)
      rate_bundle <- context$new_http_client(project$config, project$store, rate_runtime, rate_wire)
      rate_response <- rate_bundle$client$api("/repos/owner/project")
      context$check("limite máximo de espera zero encerra 429 sem retry ou pausa implícita",
        rate_response$status == 429L && rate_wire$request_count() == 1L && rate_runtime$now() == 4000)
    },
    json_decoding = function(context) {
      project <- context$new_test_project()
      bom_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(5000),
        MockHttpTransport$new("json_bom"))
      bom_response <- bom_bundle$client$api("/repos/owner/project")
      context$check("resposta JSON com BOM é interpretada sem alterar o status HTTP",
        bom_response$status == 200L && isTRUE(bom_response$body$ok))
      malformed_bundle <- context$new_http_client(project$config, project$store, MockRuntime$new(6000),
        MockHttpTransport$new("malformed_json"))
      malformed_response <- malformed_bundle$client$api("/repos/owner/project")
      context$check("JSON inválido em HTTP 200 é tratado como resposta falha",
        !malformed_bundle$client$response_ok(malformed_response) && nzchar(malformed_response$error))
    }
  ),
  lock_class = TRUE,
  cloneable = FALSE
)
