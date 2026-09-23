(() => {
  "use strict";

  const LANGUAGE_KEY = "privacy-site-language";
  const THEME_KEY = "privacy-site-theme";
  const LANGUAGES = ["pt-BR", "en-US"];
  const THEMES = ["auto", "light", "dark"];
  const locales = new Map();
  const localeRequests = new Map();

  const data = () => window.privacySiteData || {};

  function currentLanguage() {
    return document.documentElement.lang === "en-US" ? "en-US" : "pt-BR";
  }

  function preferredLanguage() {
    try {
      const saved = localStorage.getItem(LANGUAGE_KEY);
      if (LANGUAGES.includes(saved)) return saved;
    } catch {}
    return (navigator.language || "").toLowerCase().startsWith("en") ? "en-US" : "pt-BR";
  }

  function systemTheme() {
    return window.matchMedia && window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "dark"
      : "light";
  }

  function preferredTheme() {
    try {
      const saved = localStorage.getItem(THEME_KEY);
      if (THEMES.includes(saved)) return saved;
    } catch {}
    return "auto";
  }

  function resolvedTheme(preference) {
    return preference === "auto" ? systemTheme() : preference;
  }

  function loadLocale(language) {
    if (locales.has(language)) return Promise.resolve(locales.get(language));
    if (!localeRequests.has(language)) {
      const url = new URL(`site/locales/${language}.json`, document.baseURI);
      localeRequests.set(language, fetch(url).then((response) => {
        if (!response.ok) throw new Error(`Falha ao carregar ${language}: HTTP ${response.status}`);
        return response.json();
      }).then((copy) => {
        locales.set(language, copy);
        return copy;
      }));
    }
    return localeRequests.get(language);
  }

  function formatNumber(value, digits = 0, language = currentLanguage()) {
    return new Intl.NumberFormat(language, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }).format(Number(value || 0));
  }

  function formatP(value, language = currentLanguage()) {
    const numeric = Number(value);
    if (!Number.isFinite(numeric)) return "—";
    if (numeric > 0 && numeric < 0.001) {
      return new Intl.NumberFormat(language, {
        notation: "scientific",
        maximumSignificantDigits: 3
      }).format(numeric);
    }
    return new Intl.NumberFormat(language, {
      minimumFractionDigits: 6,
      maximumFractionDigits: 6
    }).format(numeric);
  }

  function renderTemplate(key, copy, language) {
    const d = data();
    const values = {
      topics: formatNumber(d.topic_count, 0, language),
      repositories: formatNumber(d.sample_rows, 0, language),
      preD1: formatNumber(d.pre_d1, 0, language),
      postD1: formatNumber(d.post_d1, 0, language),
      pairs: formatNumber(d.complete_pairs, 0, language),
      preScore: formatNumber(d.pre_score, 3, language),
      postScore: formatNumber(d.post_score, 3, language),
      mcnemar: formatP(d.mcnemar_p, language),
      wilcoxon: formatP(d.wilcoxon_p, language)
    };
    if (key === "selection-manifest") {
      key = d.selection_audit_available ? "selection-manifest-available" : "selection-manifest-pending";
    }
    const template = copy.templates[key] || "";
    return template.replace(/\{([A-Za-z][A-Za-z0-9]*)\}/g, (match, name) => values[name] ?? match);
  }

  function setText(selector, value, html = true) {
    document.querySelectorAll(selector).forEach((element) => {
      if (html) element.innerHTML = value;
      else element.textContent = value;
    });
  }

  function applyCopy(copy, language) {
    document.documentElement.lang = language;
    document.title = copy.title;

    document.querySelectorAll("[data-i18n]").forEach((element) => {
      const value = copy[element.dataset.i18n];
      if (value) element.innerHTML = value;
    });

    setText("#title-block-header .title", copy.title, false);
    setText("#title-block-header .subtitle", copy.subtitle, false);
    document.documentElement.style.setProperty("--brand-tagline", JSON.stringify(copy.brandTagline));
    setText("#hero-note", copy.heroNote);
    setText("#visao-geral h2", copy.overview, false);
    setText("#resultado-principal h2", copy.mainResult, false);
    setText("#cobertura-vocabulario h2", copy.vocabularyCoverage, false);
    setText("#resultados h2", copy.detailedResults, false);
    setText("#dados h2", copy.collectedData, false);
    setText("#metodologia h2", copy.methodology, false);
    setText("#reproducao h2", copy.reproduction, false);
    setText("#referencias-metodologicas h2", copy.methodologicalReferences, false);

    const metricLabels = [copy.sampleRepositories, copy.completePairs, copy.postD1, copy.postScore];
    document.querySelectorAll("#visao-geral .metric-card p").forEach((element, index) => {
      if (metricLabels[index]) element.textContent = metricLabels[index];
    });

    const headings = {
      "#resultados h3:nth-of-type(1)": copy.documentaryPresence,
      "#resultados h3:nth-of-type(2)": copy.pdeScore,
      "#resultados h3:nth-of-type(3)": copy.criteriaFrequency,
      "#resultados h3:nth-of-type(4)": copy.transitions,
      "#dados h3:nth-of-type(1)": copy.sample,
      "#dados h3:nth-of-type(2)": copy.inspectionFiles,
      "#metodologia h3:nth-of-type(1)": copy.design,
      "#metodologia h3:nth-of-type(2)": copy.sampleSelection,
      "#metodologia h3:nth-of-type(3)": copy.documentationCriteria,
      "#metodologia h3:nth-of-type(4)": copy.statistics,
      "#reproducao h3:nth-of-type(1)": copy.requirements,
      "#reproducao h3:nth-of-type(2)": copy.completePipeline,
      "#reproducao h3:nth-of-type(3)": copy.artifacts
    };
    Object.entries(headings).forEach(([selector, value]) => setText(selector, value, false));

    document.querySelectorAll("[data-i18n-template]").forEach((element) => {
      element.innerHTML = renderTemplate(element.dataset.i18nTemplate, copy, language);
    });

    document.querySelectorAll("table caption, table th, table td:first-child").forEach((element) => {
      const key = element.dataset.i18nKey || element.textContent.trim();
      element.dataset.i18nKey = key;
      if (copy.tableLabels[key]) element.textContent = copy.tableLabels[key];
    });
    const pValues = {
      "p exato de McNemar bicaudal": data().mcnemar_p,
      "p exato de Wilcoxon bicaudal": data().wilcoxon_p
    };
    document.querySelectorAll("table tbody tr").forEach((row) => {
      const value = pValues[row.cells[0]?.dataset.i18nKey];
      if (value !== undefined && row.cells.length > 1) {
        row.cells[row.cells.length - 1].textContent = formatP(value, language);
      }
    });

    setText("#TOC #toc-title", copy.toc, false);
    const tocLabels = {
      "#visao-geral": copy.overview,
      "#resultado-principal": copy.mainResult,
      "#cobertura-vocabulario": copy.vocabularyCoverage,
      "#resultados": copy.detailedResults,
      "#dados": copy.collectedData,
      "#metodologia": copy.methodology,
      "#reproducao": copy.reproduction,
      "#referencias-metodologicas": copy.methodologicalReferences
    };
    document.querySelectorAll("#TOC a").forEach((link) => {
      const hash = new URL(link.href, document.baseURI).hash;
      if (tocLabels[hash]) link.textContent = tocLabels[hash];
    });

    setText(".nav-footer-left", copy.footerLeft, false);
    setText(".nav-footer-right", copy.footerRight, false);
    const nav = [
      ["#resultados", copy.navResults], ["#dados", copy.navData],
      ["#metodologia", copy.navMethodology], ["#reproducao", copy.navReproduction]
    ];
    document.querySelectorAll("#quarto-header .nav-link").forEach((link, index) => {
      const href = link.getAttribute("href") || "";
      if (index === 0 || href.endsWith("index.qmd") || href.endsWith("index.html") || href === "./") {
        link.textContent = copy.navHome;
      }
      nav.forEach(([fragment, label]) => {
        if (href.includes(fragment)) link.textContent = label;
      });
    });

    document.querySelectorAll("[data-language-choice]").forEach((button) => {
      const choice = button.dataset.languageChoice;
      button.classList.toggle("is-active", choice === language);
      button.setAttribute("aria-pressed", String(choice === language));
      button.setAttribute("aria-label", copy.languageButton.replace("{language}", copy.languageNames[choice]));
    });
    const controls = document.querySelector(".site-controls");
    if (controls) {
      controls.setAttribute("aria-label", copy.controlGroup);
      const label = controls.querySelector("[data-control-label='language']");
      if (label) label.textContent = copy.languageLabel;
    }
  }

  function themeLabel(preference, copy) {
    const theme = preference === "auto" ? copy.themeAuto : preference === "light" ? copy.themeLight : copy.themeDark;
    return copy.themeButton.replace("{theme}", theme);
  }

  function applyTheme(preference, persist = true) {
    const theme = resolvedTheme(preference);
    document.documentElement.dataset.themePreference = preference;
    document.documentElement.dataset.theme = theme;
    document.documentElement.style.colorScheme = theme;
    const button = document.querySelector("[data-theme-toggle]");
    const copy = locales.get(currentLanguage()) || locales.get("pt-BR");
    if (button) {
      button.textContent = theme === "dark" ? "☾" : "☀";
      if (copy) {
        button.setAttribute("aria-label", themeLabel(preference, copy));
        button.setAttribute("title", themeLabel(preference, copy));
      }
      button.dataset.themePreference = preference;
    }
    if (persist) {
      try { localStorage.setItem(THEME_KEY, preference); } catch {}
    }
  }

  async function applyLanguage(language, persist = true) {
    const next = LANGUAGES.includes(language) ? language : "pt-BR";
    const copy = await loadLocale(next);
    applyCopy(copy, next);
    applyTheme(document.documentElement.dataset.themePreference || preferredTheme(), false);
    if (persist) {
      try { localStorage.setItem(LANGUAGE_KEY, next); } catch {}
    }
  }

  function addControls() {
    if (document.querySelector(".site-controls")) return;
    const host = document.querySelector("#quarto-header .quarto-navbar-tools") ||
      document.querySelector("#quarto-header .navbar-collapse");
    if (!host) return;
    const controls = document.createElement("div");
    controls.className = "site-controls";
    controls.setAttribute("role", "group");
    controls.innerHTML = `
      <span class="site-control-label" data-control-label="language"></span>
      <button type="button" class="site-language" data-language-choice="pt-BR">PT</button>
      <button type="button" class="site-language" data-language-choice="en-US">EN</button>
      <button type="button" class="site-theme-toggle" data-theme-toggle>☀</button>`;
    host.appendChild(controls);
    controls.querySelectorAll("[data-language-choice]").forEach((button) => {
      button.addEventListener("click", () => { void applyLanguage(button.dataset.languageChoice); });
    });
    controls.querySelector("[data-theme-toggle]").addEventListener("click", () => {
      const current = document.documentElement.dataset.themePreference || "auto";
      const next = THEMES[(THEMES.indexOf(current) + 1) % THEMES.length];
      applyTheme(next);
    });
  }

  function setNavigationTargets() {
    const brandLink = document.querySelector("#quarto-header .navbar-brand");
    if (brandLink) brandLink.href = "https://diegopn.github.io/";
    const homeLink = document.querySelector("#quarto-header .navbar-nav .nav-link");
    if (homeLink) homeLink.setAttribute("href", "#title-block-header");
  }

  async function init() {
    setNavigationTargets();
    addControls();
    try {
      await applyLanguage(preferredLanguage(), false);
    } catch (error) {
      console.error("Não foi possível carregar o idioma selecionado.", error);
      if (currentLanguage() !== "pt-BR") {
        try { await applyLanguage("pt-BR", false); } catch (fallbackError) {
          console.error("Não foi possível carregar as traduções do site.", fallbackError);
        }
      }
    }
    const media = window.matchMedia ? window.matchMedia("(prefers-color-scheme: dark)") : null;
    if (media) media.addEventListener?.("change", () => {
      if ((document.documentElement.dataset.themePreference || "auto") === "auto") applyTheme("auto", false);
    });
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", () => { void init(); });
  else void init();
})();
