TEXT_EXTENSIONS <- c(
  ".md", ".markdown", ".mdown", ".mkdn", ".rst", ".txt", ".adoc",
  ".asciidoc", ".html", ".htm", ".textile", ".xml", ".yaml", ".yml"
)

PRIVACY_MARKERS <- c(
  "privacy", "security", "gdpr", "personal-data", "personal_data",
  "data-protection", "data_protection", "policy", "legal", "terms",
  "consent", "retention", "subprocessor", "compliance"
)
RULES <- list(
  C1 = list(
    label = "dados pessoais",
    primary = c(
      "\\bpersonal data\\b", "\\bpersonal information\\b",
      "personally identifiable information", "\\bPII\\b", "data subject",
      "user data", "customer data", "email address", "ip address",
      "mailing address", "usage data", "cookies?", "license plate data",
      "authentication credentials"
    ),
    context = c("data", "information", "user", "customer", "subject", "privacy"),
    required_groups = list(c(
      "\\bpersonal data\\b", "\\bpersonal information\\b",
      "personally identifiable information", "\\bPII\\b", "data subject",
      "user data", "customer data", "privacy"
    ))
  ),
  C2 = list(
    label = "finalidade do tratamento",
    primary = c(
      "\\bpurposes?\\b", "\\bused?\\s+(?:to|for)\\b",
      "\\butili[sz](?:e|ed|ing)\\b", "use (?:the |your )?(?:data|information)",
      "(?:personal|user|customer|usage) data[^.\\n]{0,160}\\b(?:to|for)\\b",
      "collect(?:ed|ing)? (?:the |your )?(?:data|information)[^.\\n]{0,160}\\b(?:to|for)\\b",
      "collect(?:ed|ing)?[^.\\n]{0,160}\\b(?:personal data|personal information|user data|your data|your information|PII|data subjects?)\\b",
      "process(?:ed|ing)?[^.\\n]{0,160}\\b(?:personal data|personal information|user data|your data|your information|PII|data subjects?)\\b",
      "why we collect", "how do we use your information"
    ),
    context = c(
      "personal", "user", "customer", "individual", "data subject", "PII",
      "privacy", "your (?:data|information)", "account"
    ),
    required_groups = list(c("data", "information", "processing", "collect"))
  ),
  C3 = list(
    label = "base legal",
    primary = c(
      "legal basis", "lawful basis", "\\bconsent\\b", "legitimate interest",
      "legal obligation", "necessary to comply"
    ),
    context = c(
      "personal", "privacy", "data subject", "PII", "your (?:data|information)",
      "account", "user"
    ),
    required_groups = list(
      c("data", "information", "processing"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "privacy", "data subject", "PII", "user data", "customer data", "account data")
    )
  ),
  C4 = list(
    label = "direitos dos titulares",
    primary = c(
      "rights? (?:of|to) (?:data )?subjects?",
      "right to (?:access|rectification|erasure|deletion|portability|object|withdraw)",
      "access,? rectif(?:y|ication),? (?:erase|erasure|delete|deletion)",
      "data subject rights", "your right", "withdraw (?:your )?consent",
      "(?:modify|access|retrieve|correct|delete)[^.;\\n]{0,100}personal data",
      "users? can delete[^.;\\n]{0,100}(?:data|accounts?)", "data deleted from"
    ),
    context = c("data", "subject", "personal", "information", "consent", "privacy", "account"),
    required_groups = list()
  ),
  C5 = list(
    label = "retenção ou exclusão",
    primary = c(
      "data retention", "retention period",
      "retain(?:ed|ing)?[^.\\n]{0,100}(?:data|information)",
      "(?:delete|erase)(?:s|d|ion|ure)?[^.\\n]{0,100}(?:data|information|cookies?)",
      "remove(?:s|d|al)?\\s+(?:the |your )?(?:personal |user )?(?:data|information|cookies?)",
      "(?:data|account|cookies?) (?:deletion|erasure|removal)", "deleted content kept"
    ),
    context = c(
      "personal", "user", "customer", "data subject", "PII",
      "your (?:data|information)", "account", "log", "record", "retention", "cookies?"
    ),
    required_groups = list(
      c("data", "information", "storage", "log", "record", "retention", "cookies?"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "data subject", "PII", "user data", "customer data", "account data", "cookies?")
    )
  ),
  C6 = list(
    label = "compartilhamento ou transferência",
    primary = c(
      "(?:share|transfer|disclos)(?:e|ed|ure|red|ring|d|ing)?[^.\\n]{0,80}(?:personal|user|customer|usage|training|private)? ?(?:data|information|records?)",
      "(?:personal|user|customer|usage|training|private) (?:data|information|records?)[^.\\n]{0,80}(?:share|transfer|disclos)",
      "(?:information|responses?|results?)[^.\\n]{0,80}(?:share|transfer|disclos)",
      "sharing of[^.\\n]{0,60}(?:data|information)", "share (?:it|them)",
      "third[- ]party", "service provider", "sub[- ]processor",
      "international transfer", "recipient"
    ),
    context = c(
      "personal", "user", "customer", "data subject", "PII",
      "your (?:data|information)", "account", "record", "privacy"
    ),
    required_groups = list(c("data", "information", "record", "responses?", "results?"))
  ),
  C7 = list(
    label = "proteção contra vazamento ou acesso não autorizado",
    primary = c(
      "unauthori[sz]ed access", "data breach", "security incident",
      "encrypt(?:ed|ion)?", "access control", "access[^.\\n]{0,100}(?:restricted|controlled)",
      "safeguard", "security measures", "secured networks?", "sandbox(?:ed|ing)?",
      "data never leaves", "privacy[- ]preserving",
      "(?:prevent|avoid)[^.\\n]{0,120}(?:PII|personal|private|sensitive|user|privacy)? ?(?:data |information )?leak",
      "PII leakage", "keep[^.\\n]{0,120}data private"
    ),
    context = c(
      "data", "information", "personal", "privacy", "breach", "user",
      "account", "record", "subject", "PII"
    ),
    required_groups = list(
      c("data", "information", "breach", "access", "encrypt", "PII", "privacy"),
      c("\\bpersonal data\\b", "\\bpersonal information\\b", "privacy", "user data", "customer data", "data subject", "account data", "PII", "private data")
    )
  )
)

