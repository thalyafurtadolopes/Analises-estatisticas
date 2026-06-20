# =============================================================================
# FIP 606 — App de Análise Estatística Interativa
# Protótipo completo com 7 abas
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(readr)
library(tidyverse)
library(broom)
library(emmeans)
library(multcomp)
library(car)
library(plotly)
library(ggpubr)

# Tentar carregar pacotes opcionais silenciosamente
try(library(gsheet), silent = TRUE)
try(library(epifitter), silent = TRUE)
try(library(DHARMa), silent = TRUE)
try(library(performance), silent = TRUE)
try(library(lme4), silent = TRUE)
try(library(lmerTest), silent = TRUE)
try(library(agricolae), silent = TRUE)
try(library(MASS), silent = TRUE)
try(library(report), silent = TRUE)
try(library(FSA), silent = TRUE)
try(library(openxlsx), silent = TRUE)
try(library(base64enc), silent = TRUE)

# =============================================================================
# TEMA E ESTILO
# =============================================================================

tema_app <- bs_theme(
  version        = 5,
  bootswatch     = "flatly",
  primary        = "#2C7A4B",
  secondary      = "#5A9E75",
  success        = "#28a745",
  info           = "#17a2b8",
  warning        = "#f0ad4e",
  danger         = "#dc3545",
  bg             = "#F8FAF9",
  fg             = "#2d3436",
  base_font      = font_google("Inter"),
  heading_font   = font_google("Outfit"),
  code_font      = font_google("JetBrains Mono"),
  font_scale     = 0.95
)

# =============================================================================
# FUNÇÕES AUXILIARES
# =============================================================================

# Wrapper para gráfico ggplot → plotly
ggplotly_wrapper <- function(p, height = 420, ...) {
  ggplotly(p, height = height, tooltip = "all", ...) |>
    layout(
      margin = list(l = 50, r = 20, t = 40, b = 60),
      font   = list(family = "Inter, sans-serif", size = 12),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           displaylogo = FALSE)
}

# Card de resultado estatístico
stat_card <- function(label, value, icon_name = "📊", color = "#2C7A4B") {
  div(
    class = "stat-card",
    style = glue::glue("border-left: 4px solid {color}; padding: 12px 16px;
                        background: white; border-radius: 8px; margin-bottom: 10px;
                        box-shadow: 0 1px 4px rgba(0,0,0,.08);"),
    div(style = "font-size: 0.78rem; color: #636e72; font-weight: 600; text-transform: uppercase;",
        paste(icon_name, label)),
    div(style = "font-size: 1.3rem; font-weight: 700; margin-top: 4px;", value)
  )
}

# Dados de exemplo embutidos
dados_exemplo <- function() {
  data.frame(
    tratamento = rep(c("Controle", "Fungicida"), each = 8),
    bloco      = rep(1:4, times = 4),
    severidade = c(42, 38, 45, 40, 44, 37, 43, 41,
                   12, 18, 15, 10, 14, 16, 11, 13),
    produtividade = c(2800, 2650, 2900, 2750, 2820, 2690, 2870, 2730,
                      3400, 3250, 3500, 3350, 3420, 3280, 3480, 3300),
    area        = rep(c("A", "B"), times = 8)
  )
}

# Aplicar transformação na variável resposta
aplicar_transformacao <- function(y, tipo, formula_lm = NULL, df = NULL) {
  if (tipo == "none") return(list(y = y, label = "Original", lambda = NA))
  if (tipo == "log") {
    y_min <- min(y, na.rm = TRUE)
    if (y_min <= 0) {
      offset <- abs(y_min) + 1
      return(list(y = log(y + offset), label = paste0("log(y + ", round(offset, 2), ")"), lambda = NA))
    }
    return(list(y = log(y), label = "log(y)", lambda = NA))
  }
  if (tipo == "sqrt") {
    y_min <- min(y, na.rm = TRUE)
    if (y_min < 0) {
      offset <- abs(y_min)
      return(list(y = sqrt(y + offset), label = paste0("sqrt(y + ", round(offset, 2), ")"), lambda = NA))
    }
    return(list(y = sqrt(y), label = "√y", lambda = NA))
  }
  if (tipo == "boxcox") {
    tryCatch({
      # Box-Cox precisa de y > 0
      y_min <- min(y, na.rm = TRUE)
      offset <- if (y_min <= 0) abs(y_min) + 1 else 0
      y_pos <- y + offset
      
      if (!is.null(formula_lm) && !is.null(df)) {
        # Substituir a variável resposta na fórmula
        resp_name <- all.vars(formula_lm)[1]
        df[[resp_name]] <- y_pos
        bc <- MASS::boxcox(formula_lm, data = df, plotit = FALSE)
      } else {
        tmp_df <- data.frame(y_pos = y_pos)
        bc <- MASS::boxcox(y_pos ~ 1, data = tmp_df, plotit = FALSE)
      }
      lambda <- bc$x[which.max(bc$y)]
      
      y_t <- if (abs(lambda) < 0.01) log(y_pos) else (y_pos^lambda - 1) / lambda
      label <- if (abs(lambda) < 0.01) {
        if (offset > 0) paste0("log(y + ", round(offset, 2), ")") else "log(y)"
      } else {
        if (offset > 0) paste0("Box-Cox(y + ", round(offset, 2), ", λ=", round(lambda, 2), ")")
        else paste0("Box-Cox(λ=", round(lambda, 2), ")")
      }
      return(list(y = y_t, label = label, lambda = lambda))
    }, error = function(e) {
      return(list(y = y, label = "Original (Box-Cox falhou)", lambda = NA))
    })
  }
  list(y = y, label = "Original", lambda = NA)
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = span(
    img(src = "https://cdn-icons-png.flaticon.com/512/2920/2920349.png",
        height = "28px", style = "margin-right: 8px;"),
    "Análise de Dados"
  ),
  theme   = tema_app,
  id      = "main_nav",
  navbar_options = navbar_options(
    underline = TRUE,
    bg        = "#2C7A4B",
    theme     = "dark"
  ),
  header = tags$head(
    tags$style(HTML("
      /* Value boxes — fontes maiores e harmônicas */
      .bslib-value-box .value-box-value {
        font-size: 2.6rem !important;
        font-weight: 700 !important;
        line-height: 1.1 !important;
      }
      .bslib-value-box .value-box-title {
        font-size: 1.05rem !important;
        font-weight: 600 !important;
        letter-spacing: 0.03em !important;
        text-transform: uppercase !important;
        opacity: 0.92 !important;
      }
      .bslib-value-box .value-box-showcase {
        font-size: 2.8rem !important;
      }
    "))
  ),


  # ===========================================================================
  # ABA 0 — HOME / INÍCIO
  # ===========================================================================
  nav_panel(
    title = tagList(icon("home"), "Início"),
    value = "tab_home",

    card(
      card_header(
        span(
          img(src = "https://cdn-icons-png.flaticon.com/512/2920/2920349.png", height = "32px", style = "margin-right: 8px;"),
          span("Análise Estatística Interativa", style = "font-size: 1.3rem; font-weight: 700; color: #2C7A4B;")
        ),
        class = "bg-light"
      ),
      card_body(
        p("Esta plataforma interativa foi desenvolvida para apoiar as análises estatísticas em fitopatologia e agronomia. Importe seus dados e comece a analisar imediatamente!"),
        hr(),
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("🚀 Passo a Passo Rápido"),
            card_body(
              tags$ol(
                tags$li("Acesse a aba ", tags$b("Dados"), " para carregar seu arquivo local, Google Sheets ou usar o conjunto de dados de exemplo."),
                tags$li("Use a aba ", tags$b("Explorar"), " para visualizar o comportamento geral e estatísticas descritivas."),
                tags$li("Escolha o método analítico adequado (Teste t, ANOVA, Regressão, Não Paramétricos ou AUDPC)."),
                tags$li("Customize e baixe seus gráficos com qualidade de publicação científica na aba ", tags$b("Editor Gráfico"), ".")
              )
            )
          ),
          card(
            card_header("📋 Resumo dos Recursos"),
            card_body(
              layout_column_wrap(
                width = 1/2,
                div(style = "margin-bottom: 8px;", tags$b("🔬 Estatística Clássica:"), " Teste t, ANOVA com múltiplos delineamentos (DIC, DBC, Fatorial, Split-plot, LMM) e comparações múltiplas."),
                div(style = "margin-bottom: 8px;", tags$b("📈 Modelagem:"), " Regressão linear/polinomial, Correlação de Pearson/Spearman e Modelos Lineares Generalizados (GLM Poisson)."),
                div(style = "margin-bottom: 8px;", tags$b("🌱 Fitopatologia:"), " Cálculo de AUDPC com testes estatísticos integrados e curvas de progresso de doenças."),
                div(style = "margin-bottom: 8px;", tags$b("🎨 Gráficos & Laudos:"), " Customização completa com exportação em alta resolução (300 DPI) e relatórios descritivos automáticos em texto.")
              )
            )
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 1 — IMPORTAR DADOS
  # ===========================================================================
  nav_panel(
    title = tagList(icon("upload", lib = "font-awesome"), " Dados"),
    value = "tab_dados",

    layout_sidebar(
      sidebar = sidebar(
        width  = 320,
        bg     = "white",
        title  = "📂 Fonte dos Dados",

        radioButtons("fonte_dados", label = NULL,
                     choices = c("📁 Arquivo local"  = "local",
                                 "🔗 Google Sheets"   = "gsheets",
                                 "🧪 Dados de exemplo" = "exemplo",
                                 "📦 Datasets do R"    = "r_datasets"),
                     selected = "exemplo"),

        # Arquivo local
        conditionalPanel(
          condition = "input.fonte_dados == 'local'",
          fileInput("arquivo", "Selecione o arquivo:",
                    accept  = c(".csv", ".xlsx", ".xls", ".tsv"),
                    buttonLabel = "Procurar...",
                    placeholder = "CSV, XLSX ou XLS"),
          selectInput("sep_csv", "Separador (CSV):",
                      choices = c("Ponto-vírgula (;)" = ";",
                                  "Vírgula (,)" = ",",
                                  "Tabulação" = "\t")),
          selectInput("dec_csv", "Separador decimal:",
                      choices = c("Vírgula (,)" = ",",
                                  "Ponto (.)" = ".")),
          uiOutput("ui_aba_excel")
        ),

        # Google Sheets
        conditionalPanel(
          condition = "input.fonte_dados == 'gsheets'",
          textInput("url_gs", "URL da planilha pública:",
                    placeholder = "https://docs.google.com/..."),
          actionButton("carregar_gs", "Carregar do Google Sheets",
                       class = "btn-success w-100", icon = icon("cloud-download-alt"))
        ),

        # Exemplo
        conditionalPanel(
          condition = "input.fonte_dados == 'exemplo'",
          div(class = "alert alert-success",
              icon("circle-info"), " Usando dados de exemplo do curso FIP 606.",
              br(), "Experimento com Controle × Fungicida (n=16)")
        ),

        # Datasets do R
        conditionalPanel(
          condition = "input.fonte_dados == 'r_datasets'",
          selectInput("r_dataset_name", "Selecione o Dataset do R:",
                      choices = c("InsectSprays", "iris", "mtcars", "CO2", "ToothGrowth", "PlantGrowth", "warpbreaks")),
          div(class = "alert alert-info p-2", style = "font-size: 0.8rem;",
              "Estes conjuntos de dados são embutidos e prontos para demonstração rápida.")
        ),

        hr(),
        h6("⚙️ Opções de importação"),
        checkboxInput("header", "Primeira linha como cabeçalho", TRUE),
        checkboxInput("stringsAsFactors", "Converter texto em fatores", FALSE),

        hr(),
        actionButton("limpar_dados", "🗑️ Limpar dados",
                     class = "btn-outline-danger btn-sm w-100")
      ),

      # Painel principal da aba Dados
      layout_columns(
        col_widths = c(4, 4, 4),

        value_box("Observações",   textOutput("n_linhas"),
                  showcase = bsicons::bs_icon("table"),
                  theme = value_box_theme(bg = "#2C7A4B", fg = "white")),
        value_box("Variáveis",     textOutput("n_colunas"),
                  showcase = bsicons::bs_icon("bar-chart"),
                  theme = value_box_theme(bg = "#5A9E75", fg = "white")),
        value_box("Tipo do arquivo", textOutput("tipo_arquivo"),
                  showcase = bsicons::bs_icon("file-earmark-spreadsheet"),
                  theme = value_box_theme(bg = "#84BD9B", fg = "white"))
      ),
      br(),
      accordion(
        id = "accordion_dados",
        open = FALSE,
        accordion_panel(
          title = "📋 Prévia dos Dados",
          DTOutput("tabela_preview")
        ),
        accordion_panel(
          title = "🔍 Estrutura das Variáveis",
          verbatimTextOutput("estrutura_dados")
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 2 — EXPLORAÇÃO
  # ===========================================================================
  nav_panel(
    title = tagList(icon("chart-bar"), " Explorar"),
    value = "tab_explorar",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "🎛️ Controles",

        accordion(
          open = c("Variáveis", "Gráfico"),
          accordion_panel(
            "Variáveis", icon = icon("table"),
            selectInput("var_resp_exp", "Variável resposta (numérica):", choices = NULL),
            selectInput("var_grupo_exp", "Grupo / Fator:", choices = NULL)
          ),
          accordion_panel(
            "Transformação", icon = icon("calculator"),
            selectInput("transf_exp", label = NULL,
                        choices = c("Nenhuma" = "none", "log(y)" = "log",
                                    "sqrt(y)" = "sqrt", "log(y+1)" = "log1p"))
          ),
          accordion_panel(
            "Gráfico", icon = icon("chart-pie"),
            checkboxGroupInput("tipo_grafico", label = NULL,
                               choices = c("Barras (Média)" = "bar",
                                           "Boxplot"        = "box",
                                           "Violino"        = "violin",
                                           "Pontos (Jitter)"= "jitter",
                                           "Ponto da Média" = "mean",
                                           "Desvio Padrão"  = "sd",
                                           "Erro Padrão"    = "se",
                                           "Letras (Tukey)" = "tukey"),
                               selected = c("bar", "jitter", "se"))
          )
        ),

        hr(),
        downloadButton("download_resumo", "Baixar resumo CSV",
                       class = "btn-outline-success btn-sm w-100")
      ),

      card(full_screen = TRUE,
        card_header("📊 Visualização por Grupo"),
        card_body(plotlyOutput("grafico_explorar", height = "550px"))
      ),
      br(),
      card(
        card_header("📋 Resumo Estatístico por Grupo"),
        card_body(DTOutput("tabela_resumo"))
      )
    )
  ),

  # ===========================================================================
  # ABA 3 — COMPARAÇÃO DE MÉDIAS (Teste t / Wilcoxon)
  # ===========================================================================
  nav_panel(
    title = tagList(icon("not-equal"), " Teste t"),
    value = "tab_ttest",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "🧪 Configuração",

        checkboxInput("usar_audpc_tt", "🌱 Usar AUDPC como variável Y", FALSE),
        conditionalPanel(
          condition = "input.usar_audpc_tt == false",
          selectInput("var_resp_tt", "Variável resposta:", choices = NULL)
        ),
        selectInput("var_grupo_tt", "Fator (2 grupos):", choices = NULL),

        hr(),
        h6("Tipo de teste"),
        radioButtons("tipo_teste", label = NULL,
                     choices = c("t de Student independente" = "t_indep",
                                 "t de Student pareado"      = "t_pareado",
                                 "Wilcoxon / Mann-Whitney"   = "wilcoxon"),
                     selected = "t_indep"),

        conditionalPanel(
          condition = "input.tipo_teste == 't_indep'",
          checkboxInput("var_equal", "Assumir variâncias iguais (Fisher)", FALSE)
        ),

        hr(),
        h6("🔄 Transformação da resposta"),
        radioButtons("transf_tt", label = NULL,
                     choices = c("Nenhuma"   = "none",
                                 "log(y)"   = "log",
                                 "√y"       = "sqrt",
                                 "Box-Cox"  = "boxcox"),
                     selected = "none", inline = TRUE),

        hr(),
        h6("Nível de significância (α)"),
        sliderInput("alpha_tt", label = NULL,
                    min = 0.01, max = 0.10, value = 0.05, step = 0.01),

        actionButton("rodar_teste", "▶ Rodar Teste",
                     class = "btn-success w-100", icon = icon("play"))
      ),

      navset_card_underline(
        title = "Resultados do Teste t",
        full_screen = TRUE,
        nav_panel("📊 Resultado",
          layout_columns(
            col_widths = c(5, 7),
            card_body(
              uiOutput("resultado_cards_tt"),
              hr(),
              verbatimTextOutput("resultado_raw_tt")
            ),
            card_body(plotlyOutput("grafico_distribuicao_t", height = "400px"))
          )
        ),
        nav_panel("📦 Dados por Grupo",
          card_body(plotlyOutput("grafico_tt_grupos", height = "500px"))
        ),
        nav_panel("🔬 Premissas",
          card_body(
            tabsetPanel(
              tabPanel("Shapiro-Wilk",    br(), verbatimTextOutput("shapiro_tt")),
              tabPanel("Levene / Bartlett", br(), verbatimTextOutput("homog_tt")),
              tabPanel("QQ-Plot",         plotlyOutput("qqplot_tt", height = "400px"))
            )
          )
        ),
        nav_panel("🔄 Dados Transformados",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            plotlyOutput("hist_transf_tt", height = "400px"),
            hr(),
            verbatimTextOutput("resumo_transf_tt")
          )
        ),
        nav_panel("📄 Relatório em Texto",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            downloadButton("download_report_tt", "Baixar Relatório (.txt)", class = "btn-outline-primary mb-2"),
            verbatimTextOutput("report_tt")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 4 — ANOVA
  # ===========================================================================
  nav_panel(
    title = tagList(icon("table"), " ANOVA"),
    value = "tab_anova",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "📐 Modelo",

        checkboxInput("usar_audpc_av", "🌱 Usar AUDPC como variável Y", FALSE),
        conditionalPanel(
          condition = "input.usar_audpc_av == false",
          selectInput("var_resp_av", "Variável resposta:", choices = NULL)
        ),

        conditionalPanel(
          condition = "input.delineamento == 'dic' || input.delineamento == 'dbc'",
          selectInput("var_trat_av", "Tratamento (fator principal):", choices = NULL)
        ),
        
        conditionalPanel(
          condition = "input.delineamento == 'dbc'",
          selectInput("var_bloco_av", "Bloco / 2º fator:", choices = NULL)
        ),
        
        conditionalPanel(
          condition = "input.delineamento == 'fatorial'",
          selectInput("fator_principal", "Fator Principal (linhas da tabela):", choices = NULL),
          selectInput("fator_desdobramento", "Fator de Desdobramento (dentro de cada):", choices = NULL),
          selectInput("desdobrar_sentido", "Sentido do Desdobramento:",
                      choices = c("Fator Principal dentro de Fator de Desdobramento" = "normal",
                                  "Fator de Desdobramento dentro de Fator Principal" = "inverso"))
        ),

        # Parcelas subdivididas
        conditionalPanel(
          condition = "input.delineamento == 'splitplot'",
          selectInput("split_fator_a", "Fator A (parcela principal):", choices = NULL),
          selectInput("split_fator_b", "Fator B (sub-parcela):", choices = NULL),
          selectInput("split_bloco", "Bloco / Repetição:", choices = NULL)
        ),

        # Modelo Linear Misto
        conditionalPanel(
          condition = "input.delineamento == 'misto'",
          selectInput("misto_fator_a", "Fator A — parcela principal (fixo):", choices = NULL),
          selectInput("misto_fator_b", "Fator B — sub-parcela (fixo):", choices = NULL),
          selectInput("misto_bloco",   "Bloco / Repetição (aleatório):", choices = NULL),
          div(class = "alert alert-secondary p-2", style = "font-size:0.80rem;",
              "Modelo:", br(),
              tags$code("Y ~ FatorA * FatorB + (1|Bloco) + (1|Bloco:FatorA)"))
        ),

        hr(),
        h6("Delineamento / Tipo de ANOVA"),
        radioButtons("delineamento", label = NULL,
                     choices = c("✅ 1 fator — ANOVA simples (DIC)"          = "dic",
                                 "🔲 1 fator + Bloco (DBC)"                   = "dbc",
                                 "🔀 2 fatores com interação (Fatorial)"      = "fatorial",
                                 "🌱 Parcelas subdivididas (Split-plot)"      = "splitplot",
                                 "🔗 Modelo linear misto (LMM)"               = "misto"),
                     selected = "dic"),
        div(class = "alert alert-info p-2 mt-1",
            style = "font-size: 0.82rem;",
            tags$b("1 fator (DIC):"), " ANOVA clássica DIC.", br(),
            tags$b("DBC:"), " inclui bloco de controle.", br(),
            tags$b("Fatorial:"), " 2 fatores e interação.", br(),
            tags$b("Split-plot:"), " fatores em parcelas e subparcelas.", br(),
            tags$b("Misto (LMM):"), " efeitos fixos e aleatórios via lmer."),

        hr(),
        h6("Comparações múltiplas"),
        selectInput("metodo_cld", label = NULL,
                    choices = c("Tukey"        = "tukey",
                                "Fisher (LSD)" = "none",
                                "Bonferroni"   = "bonferroni",
                                "Sidak"        = "sidak")),

        sliderInput("alpha_av", "α:", min = 0.01, max = 0.10,
                    value = 0.05, step = 0.01),

        hr(),
        h6("🔄 Transformação da resposta"),
        radioButtons("transf_av", label = NULL,
                     choices = c("Nenhuma"   = "none",
                                 "log(y)"   = "log",
                                 "√y"       = "sqrt",
                                 "Box-Cox"  = "boxcox"),
                     selected = "none", inline = TRUE),

        actionButton("rodar_anova", "▶ Rodar ANOVA",
                     class = "btn-success w-100", icon = icon("play"))
      ),

      navset_card_underline(
        title = "Resultados da ANOVA",
        full_screen = TRUE,
        nav_panel("📋 Tabela ANOVA",
          card_body(DTOutput("tabela_anova"))
        ),
        nav_panel("🔬 Premissas (DHARMa)",
          card_body(
            tabsetPanel(
              tabPanel("Gráficos Residuais", plotOutput("plot_dharma_av", height = "450px")),
              tabPanel("Testes Formais",     br(), verbatimTextOutput("testes_dharma_av"))
            )
          )
        ),
        nav_panel("📊 Médias Ajustadas",
          card_body(plotlyOutput("grafico_emmeans", height = "500px"))
        ),
        nav_panel("📋 Tabela de Médias",
          card_body(DTOutput("tabela_emmeans"))
        ),
        nav_panel("🔄 Dados Transformados",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            plotlyOutput("hist_transf_av", height = "400px"),
            hr(),
            verbatimTextOutput("resumo_transf_av")
          )
        ),
        nav_panel("📄 Relatório em Texto",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            downloadButton("download_report_av", "Baixar Relatório (.txt)", class = "btn-outline-primary mb-2"),
            verbatimTextOutput("report_av")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 5 — REGRESSÃO E CORRELAÇÃO
  # ===========================================================================
  nav_panel(
    title = tagList(icon("chart-line"), " Regressão"),
    value = "tab_reg",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "📈 Configuração",

        selectInput("var_x_reg", "Variável X (independente):", choices = NULL),
        selectInput("var_y_reg", "Variável Y (dependente):", choices = NULL),
        selectInput("var_cor_reg", "Agrupar por — group_by (opcional):", choices = NULL),

        hr(),
        h6("Tipo de análise"),
        radioButtons("tipo_reg", label = NULL,
                     choices = c("Correlação de Pearson"    = "pearson",
                                 "Correlação de Spearman"   = "spearman",
                                 "Regressão Linear (1º grau)"  = "linear",
                                 "Regressão Polinomial (2º)" = "poly2",
                                 "Regressão Polinomial (3º)" = "poly3"),
                     selected = "linear"),

        hr(),
        checkboxInput("mostrar_ic_reg", "Mostrar IC 95%", TRUE),
        checkboxInput("mostrar_eq_reg", "Anotar equação no gráfico", TRUE),

        actionButton("rodar_reg", "▶ Analisar",
                     class = "btn-success w-100", icon = icon("play"))
      ),

      navset_card_underline(
        title = "Resultados da Regressão",
        full_screen = TRUE,
        nav_panel("📊 Histogramas (Distribuições)",
          card_body(
            div(class = "alert alert-info p-2", style = "font-size:0.85rem;",
                tags$b("🔍 Inspeção das distribuições:"),
                " Pearson e regressão linear assumem distribuição aproximadamente normal e ausência de assimetria severa."),
            plotlyOutput("histogramas_reg", height = "550px")
          )
        ),
        nav_panel("📊 Gráfico de Dispersão",
          card_body(plotlyOutput("grafico_reg", height = "500px"))
        ),
        nav_panel("📋 Resultados e Equação",
          card_body(
            uiOutput("resultado_reg_cards"),
            hr(),
            verbatimTextOutput("resultado_reg_raw"),
            conditionalPanel(
              condition = "input.tipo_reg == 'poly2'",
              hr(),
              h5("🎯 Ponto de Máximo/Mínimo"),
              uiOutput("ponto_otimo")
            )
          )
        ),
        nav_panel("📄 Relatório em Texto",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            downloadButton("download_report_reg", "Baixar Relatório (.txt)", class = "btn-outline-primary mb-2"),
            verbatimTextOutput("report_reg")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 6 — GLM / DADOS DE CONTAGEM
  # ===========================================================================
  nav_panel(
    title = tagList(icon("bug"), " GLM"),
    value = "tab_glm",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "🦠 Dados de Contagem",

        checkboxInput("usar_audpc_glm", "🌱 Usar AUDPC como variável Y", FALSE),
        conditionalPanel(
          condition = "input.usar_audpc_glm == false",
          selectInput("var_resp_glm", "Variável resposta (contagem):", choices = NULL)
        ),
        selectInput("var_grupo_glm", "Fator:", choices = NULL),

        hr(),
        h6("Abordagens a comparar"),
        checkboxGroupInput("modelos_glm", label = NULL,
                           choices = c("ANOVA (dados brutos)"  = "lm_bruto",
                                       "ANOVA (√ raiz)"        = "lm_sqrt",
                                       "Kruskal-Wallis"        = "kruskal",
                                       "GLM Poisson"           = "glm_poisson"),
                           selected = c("lm_bruto", "glm_poisson")),

        hr(),
        selectInput("metodo_cld_glm", "Comparações múltiplas:",
                    choices = c("Tukey" = "tukey", "Fisher (LSD)" = "none")),

        actionButton("rodar_glm", "▶ Rodar Análise",
                     class = "btn-success w-100", icon = icon("play"))
      ),

      navset_card_underline(
        title = "Resultados GLM",
        full_screen = TRUE,
        nav_panel("📋 Comparação de Modelos",
          card_body(DTOutput("tabela_comparacao_glm"))
        ),
        nav_panel("📊 Médias Estimadas (Poisson)",
          card_body(plotlyOutput("grafico_emmeans_glm", height = "500px"))
        ),
        nav_panel("📊 Gráfico Exploratório",
          card_body(plotlyOutput("grafico_explorar_glm", height = "500px"))
        ),
        nav_panel("🔬 Detalhes da ANOVA",
          card_body(
            tabsetPanel(
              tabPanel("ANOVA Bruta",   br(), verbatimTextOutput("res_lm_bruto")),
              tabPanel("ANOVA √",       br(), verbatimTextOutput("res_lm_sqrt")),
              tabPanel("Kruskal-Wallis", br(), verbatimTextOutput("res_kruskal")),
              tabPanel("GLM Poisson",   br(), verbatimTextOutput("res_glm_poisson"))
            )
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 7 — AUDPC / PROGRESSÃO DE DOENÇAS
  # ===========================================================================
  nav_panel(
    title = tagList(icon("virus"), " AUDPC"),
    value = "tab_audpc",

    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "🦠 Curva de Progresso",

        div(class = "alert alert-info",
            icon("circle-info"),
            " Esta aba calcula a AUDPC (Área sob a Curva de Progresso da Doença)."),

        selectInput("var_tempo_audpc",  "Coluna de tempo:",       choices = NULL),
        selectInput("var_sev_audpc",    "Coluna de severidade:",  choices = NULL),
        selectInput("var_grupo_audpc",  "Grupo / Tratamento:",    choices = NULL),
        selectInput("var_rep_audpc",    "Repetição (opcional):",  choices = NULL),

        hr(),
        h6("Escala da severidade"),
        radioButtons("escala_sev_audpc", label = NULL,
                     choices = c("Proporção (0–1)" = "prop",
                                 "Percentual (0–100)" = "pct")),

        actionButton("calcular_audpc", "▶ Calcular AUDPC",
                     class = "btn-success w-100", icon = icon("calculator"))
      ),

      navset_card_underline(
        title = "Resultados AUDPC",
        full_screen = TRUE,
        nav_panel("📈 Curva de Progresso",
          card_body(plotlyOutput("grafico_curva_doenca", height = "500px"))
        ),
        nav_panel("📊 Comparação AUDPC",
          card_body(plotlyOutput("grafico_audpc_comp", height = "500px"))
        ),
        nav_panel("📋 Tabela AUDPC",
          card_body(
            DTOutput("tabela_audpc"),
            hr(),
            h5("Teste Estatístico Global:"),
            verbatimTextOutput("teste_audpc")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 8 — TESTES NÃO PARAMÉTRICOS
  # ===========================================================================
  nav_panel(
    title = tagList(icon("balance-scale"), " Não Paramétricos"),
    value = "tab_nparam",
    layout_sidebar(
      sidebar = sidebar(
        width = 280, bg = "white",
        title = "🧪 Configuração",
        checkboxInput("usar_audpc_np", "🌱 Usar AUDPC como variável Y", FALSE),
        conditionalPanel(
          condition = "input.usar_audpc_np == false",
          selectInput("var_resp_np", "Variável resposta:", choices = NULL)
        ),
        selectInput("var_grupo_np", "Fator (Agrupamento):", choices = NULL),
        hr(),
        h6("Tipo de teste"),
        radioButtons("tipo_teste_np", label = NULL,
                     choices = c("Mann-Whitney (2 indep.)" = "mann",
                                 "Wilcoxon Pareado (2 dep.)" = "wilcox_par",
                                 "Kruskal-Wallis (>2 indep.)" = "kruskal",
                                 "Friedman (>2 dep.)" = "friedman"),
                     selected = "mann"),
        conditionalPanel(
          condition = "input.tipo_teste_np == 'friedman'",
          selectInput("var_bloco_np", "Bloco/Sujeito (para Friedman):", choices = NULL)
        ),
        hr(),
        actionButton("rodar_np", "▶ Analisar", class = "btn-success w-100", icon = icon("play"))
      ),
      navset_card_underline(
        title = "Resultados Não Paramétricos", full_screen = TRUE,
        nav_panel("📊 Boxplots", card_body(plotlyOutput("plot_np", height = "500px"))),
        nav_panel("📋 Estatística do Teste", 
          card_body(
            verbatimTextOutput("resumo_np"),
            conditionalPanel(
              condition = "input.tipo_teste_np == 'kruskal'",
              hr(),
              h5("Teste Post-Hoc de Dunn (Comparações Múltiplas)"),
              verbatimTextOutput("posthoc_np")
            ),
            conditionalPanel(
              condition = "input.tipo_teste_np == 'friedman'",
              hr(),
              h5("Teste Post-Hoc de Nemenyi"),
              verbatimTextOutput("posthoc_friedman")
            )
          )
        ),
        nav_panel("📄 Relatório em Texto",
          card_body(fill = FALSE, fillable = FALSE, style = "overflow-y: auto;",
            downloadButton("download_report_np", "Baixar Relatório (.txt)", class = "btn-outline-primary mb-2"),
            verbatimTextOutput("report_np")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 9 — EDITOR DE GRÁFICOS
  # ===========================================================================
  nav_panel(
    title = tagList(icon("paint-brush"), " Editor Gráfico"),
    value = "tab_graficos",
    layout_sidebar(
      sidebar = sidebar(
        width = 300, bg = "white",
        title = "🎨 Customização",
        selectInput("fonte_grafico", "Origem dos Dados:",
                    choices = c(
                      "📐 ANOVA"                   = "anova",
                      "🧪 Teste t / Wilcoxon"      = "teste_t",
                      "📈 Regressão / Correlação"  = "regressao",
                      "🦠 GLM"             = "glm",
                      "⚖️ Não Paramétricos"        = "nao_param",
                      "🌱 AUDPC"                   = "audpc"
                    )
        ),
        hr(),
        h6("Rótulos"),
        textInput("graf_xlab", "Título Eixo X:", value = "Tratamentos"),
        textInput("graf_ylab", "Título Eixo Y:", value = "Média"),
        textInput("graf_title", "Título Principal:", value = ""),
        hr(),
        h6("Barras de Erro e Letras"),
        selectInput("graf_tipo", "Tipo de Gráfico:", 
                    choices = c("Colunas" = "coluna", "Boxplot" = "boxplot", "Dispersão/Linhas" = "ponto"), 
                    selected = "coluna"),
        radioButtons("graf_erro", "Tipo de Erro:", 
                     choices = c("Desvio Padrão (SD)" = "sd", 
                                 "Erro Padrão (SE)" = "se", 
                                 "Intervalo (IC 95%)" = "ci"), selected = "se"),
        checkboxInput("graf_letras", "Mostrar letras de médias", value = TRUE),
        checkboxInput("graf_valores", "Mostrar valor da média numérica", value = FALSE),
        hr(),
        h6("Estilo"),
        sliderInput("graf_font", "Tamanho da fonte:", min = 8, max = 24, value = 14),
        selectInput("graf_angle", "Rotação eixo X:", choices = c("0º" = 0, "45º" = 45, "90º" = 90), selected = 0),
        actionButton("atualizar_grafico", "🔄 Gerar/Atualizar Gráfico", class = "btn-primary w-100")
      ),
      card(
        card_header("Gráfico Publicável"),
        card_body(
          plotOutput("plot_custom", height = "500px"),
          downloadButton("download_grafico", "⬇ Baixar Imagem (PNG)", class = "btn-success mt-3")
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 10 E 11 — RELATÓRIO E EXPORTAR (AGRUPADOS)
  # ===========================================================================
  nav_menu(
    title = "Saída e Exportação",
    icon = icon("save"),

    nav_panel(
      title = tagList(icon("file-alt"), " Relatório"),
      value = "tab_relatorio",

    layout_sidebar(
      sidebar = sidebar(
        width = 310, bg = "white",
        title = "📄 Configurações do Relatório",

        h6("📝 Identificação"),
        textInput("rel_titulo",      "Título do Experimento:",
                  value = "Análise Estatística — FIP 606"),
        textInput("rel_autor",       "Autor(es):",        value = ""),
        textInput("rel_instituicao", "Instituição:",      value = "UFV — FIP 606"),
        textInput("rel_data",        "Data:",
                  value = format(Sys.Date(), "%d/%m/%Y")),

        hr(),
        h6("☑️ Seções a incluir"),
        checkboxInput("rel_inc_dados",   "📋 Resumo dos Dados",            TRUE),
        checkboxInput("rel_inc_exp",     "📊 Exploração / Descritiva",     TRUE),
        checkboxInput("rel_inc_tt",      "🧪 Teste t / Wilcoxon",          TRUE),
        checkboxInput("rel_inc_anova",   "📐 ANOVA",                        TRUE),
        checkboxInput("rel_inc_reg",     "📈 Regressão / Correlação",      TRUE),
        checkboxInput("rel_inc_glm",     "🦠 GLM",                          TRUE),
        checkboxInput("rel_inc_np",      "⚖️ Não Paramétricos",            TRUE),
        checkboxInput("rel_inc_audpc",   "🌱 AUDPC",                        TRUE),
        checkboxInput("rel_inc_graficos","🎨 Gráfico Customizado (Editor)", TRUE),

        hr(),
        actionButton("gerar_relatorio", "▶ Gerar Relatório",
                     class = "btn-success w-100", icon = icon("cogs")),
        br(), br(),
        downloadButton("download_html", "⬇ Baixar HTML",
                       class = "btn-primary w-100"),
        br(), br(),
        div(class = "alert alert-info p-2", style = "font-size:0.82rem;",
            icon("info-circle"),
            " O arquivo HTML é auto-contido. ",
            "Para gerar PDF, baixe o HTML, abra-o no seu navegador e use ", tags$b("Ctrl+P (Imprimir -> Salvar como PDF)."))
      ),

      card(
        full_screen = TRUE,
        card_header("👁️ Pré-visualização do Relatório"),
        card_body(
          uiOutput("status_relatorio"),
          uiOutput("preview_relatorio_iframe",
                     style = "border:1px solid #dee2e6; border-radius:6px; background:white; padding:5px;")
        )
      )
    )
  ),

  # ===========================================================================
  # ABA 11 — EXPORTAR DADOS
  # ===========================================================================
  nav_panel(
    title = tagList(icon("download"), " Exportar"),
    value = "tab_exportar",

    layout_sidebar(
      sidebar = sidebar(
        width = 300, bg = "white",
        title = "📦 O que exportar?",

        h6("Selecione a tabela:"),
        radioButtons("export_fonte", label = NULL,
          choices = c(
            "📋 Dados brutos"                = "brutos",
            "📊 Resumo descritivo (Explorar)"= "resumo",
            "🧪 Resultados do Teste t"       = "teste_t",
            "📐 Tabela ANOVA"                = "anova",
            "📋 Médias ajustadas (emmeans)"  = "emmeans",
            "📈 Regressão / Correlação"      = "regressao",
            "🦠 GLM Poisson (Médias)"        = "glm",
            "🌱 Tabela AUDPC"                = "audpc",
            "⚖️ Resultados Não Paramétricos" = "nao_param"
          ),
          selected = "brutos"
        ),

        hr(),
        h6("Formato de exportação:"),
        radioButtons("export_formato", label = NULL,
          choices = c("CSV (.csv)" = "csv",
                      "Excel (.xlsx)" = "xlsx",
                      "Texto (.txt)" = "txt"),
          selected = "csv"
        ),

        conditionalPanel(
          condition = "input.export_formato == 'csv'",
          selectInput("export_sep", "Separador:",
                      choices = c(";" = ";", "," = ",", "Tab" = "\t")),
          selectInput("export_dec", "Dec. separador:",
                      choices = c("," = ",", "." = "."))
        ),

        hr(),
        downloadButton("download_exportar", "⬇ Baixar Tabela",
                       class = "btn-success w-100")
      ),

      card(
        full_screen = TRUE,
        card_header("👁️ Pré-visualização da Tabela"),
        card_body(DTOutput("preview_exportar"))
      )
    )
  )
  ),

  # Separador e Info
  nav_spacer(),
  nav_menu(
    title = "ℹ️ Sobre",
    nav_item(
      tags$a("📚 Código do Curso (GitHub)", href = "#", target = "_blank")
    ),
    nav_item(
      actionLink("about_link", "ℹ️ Sobre o App")
    ),
    nav_item(
      tags$a("🔗 LinkedIn: Maria Eduarda", href = "https://www.linkedin.com/in/maria-eduarda-faria-tardim-86683b218/", target = "_blank")
    ),
    nav_item(
      tags$a("🔗 LinkedIn: Thalya", href = "https://www.linkedin.com/in/thalya-furtado-lopes-90a3232a9/", target = "_blank")
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # REATIVO: dados carregados
  # ---------------------------------------------------------------------------

  dados <- reactiveVal(NULL)

  observeEvent(c(input$fonte_dados, input$r_dataset_name), {
    if (input$fonte_dados == "exemplo") {
      dados(dados_exemplo())
    } else if (input$fonte_dados == "r_datasets") {
      req(input$r_dataset_name)
      tryCatch({
        df <- get(input$r_dataset_name, envir = as.environment("package:datasets"))
        dados(as.data.frame(df))
      }, error = function(e) {
        df <- switch(input$r_dataset_name,
                     "InsectSprays" = InsectSprays,
                     "iris"         = iris,
                     "mtcars"       = mtcars,
                     "CO2"          = CO2,
                     "ToothGrowth"  = ToothGrowth,
                     "PlantGrowth"  = PlantGrowth,
                     "warpbreaks"   = warpbreaks)
        dados(as.data.frame(df))
      })
    }
  }, ignoreInit = FALSE)

  observeEvent(c(input$arquivo, input$aba_excel, input$sep_csv, input$dec_csv), {
    req(input$arquivo)
    ext <- tools::file_ext(input$arquivo$name)
    tryCatch({
      df <- if (ext %in% c("xlsx", "xls")) {
        aba <- if (!is.null(input$aba_excel) && input$aba_excel != "") input$aba_excel else 1
        read_excel(input$arquivo$datapath, sheet = aba)
      } else {
        read_delim(input$arquivo$datapath, delim = input$sep_csv,
                   locale = locale(decimal_mark = input$dec_csv),
                   col_names = input$header, show_col_types = FALSE)
      }
      dados(as.data.frame(df))
    }, error = function(e) {
      showNotification(paste("Erro ao importar:", e$message), type = "error")
    })
  })

  observeEvent(input$carregar_gs, {
    req(input$url_gs)
    tryCatch({
      df <- gsheet::gsheet2tbl(input$url_gs)
      dados(as.data.frame(df))
      showNotification("✅ Planilha carregada com sucesso!", type = "message")
    }, error = function(e) {
      showNotification(paste("Erro Google Sheets:", e$message), type = "error")
    })
  })

  observeEvent(input$limpar_dados, {
    dados(NULL)
    showNotification("Dados removidos.", type = "warning")
  })

  # Abas do Excel
  output$ui_aba_excel <- renderUI({
    req(input$arquivo)
    ext <- tools::file_ext(input$arquivo$name)
    if (ext %in% c("xlsx", "xls")) {
      abas <- tryCatch(excel_sheets(input$arquivo$datapath), error = function(e) NULL)
      if (!is.null(abas))
        selectInput("aba_excel", "Aba da planilha:", choices = abas)
    }
  })

  # Colunas numéricas e de fator
  colunas_num <- reactive({
    req(dados())
    names(dados())[sapply(dados(), is.numeric)]
  })
  colunas_all <- reactive({
    req(dados())
    names(dados())
  })
  colunas_fator <- reactive({
    req(dados())
    cols <- names(dados())
    # Inclui todas as colunas com <= 15 valores únicos ou que sejam character/factor
    cols[sapply(dados(), function(x) is.character(x) | is.factor(x) |
                  (is.numeric(x) & length(unique(x)) <= 15))]
  })

  # ---------------------------------------------------------------------------
  # Atualizar selectInputs em todas as abas quando dados mudam
  # ---------------------------------------------------------------------------

  observe({
    req(dados())
    num <- colunas_num()
    all <- colunas_all()
    fat <- colunas_fator()
    opc_none <- c("(nenhum)" = "")

    updateSelectInput(session, "var_resp_exp",   choices = num)
    updateSelectInput(session, "var_grupo_exp",  choices = c(opc_none, fat))

    updateSelectInput(session, "var_resp_tt",    choices = num)
    updateSelectInput(session, "var_grupo_tt",   choices = fat)

    updateSelectInput(session, "var_resp_av",    choices = num)
    updateSelectInput(session, "var_trat_av",    choices = fat)
    updateSelectInput(session, "var_bloco_av",   choices = fat)
    
    # Defaults para evitar selecionar o mesmo fator por padrao
    fator_p_sel <- if (length(fat) >= 1) fat[1] else NULL
    fator_d_sel <- if (length(fat) >= 2) fat[2] else (if (length(fat) >= 1) fat[1] else NULL)
    updateSelectInput(session, "fator_principal", choices = fat, selected = fator_p_sel)
    updateSelectInput(session, "fator_desdobramento", choices = fat, selected = fator_d_sel)

    # Atualizações para os novos delineamentos da ANOVA
    updateSelectInput(session, "split_fator_a", choices = fat, selected = fator_p_sel)
    updateSelectInput(session, "split_fator_b", choices = fat, selected = fator_d_sel)
    updateSelectInput(session, "split_bloco",   choices = fat)
    updateSelectInput(session, "misto_fator_a", choices = fat, selected = fator_p_sel)
    updateSelectInput(session, "misto_fator_b", choices = fat, selected = fator_d_sel)
    updateSelectInput(session, "misto_bloco",   choices = fat)

    updateSelectInput(session, "var_x_reg",      choices = num)
    updateSelectInput(session, "var_y_reg",      choices = rev(num))
    updateSelectInput(session, "var_cor_reg",    choices = c(opc_none, all))

    updateSelectInput(session, "var_resp_glm",   choices = num)
    updateSelectInput(session, "var_grupo_glm",  choices = fat)

    # Aba 8 - Não Paramétricos
    updateSelectInput(session, "var_resp_np",    choices = num)
    updateSelectInput(session, "var_grupo_np",   choices = fat)
    updateSelectInput(session, "var_bloco_np",   choices = fat)

    updateSelectInput(session, "var_tempo_audpc", choices = num)
    updateSelectInput(session, "var_sev_audpc",   choices = num)
    updateSelectInput(session, "var_grupo_audpc", choices = fat)
    updateSelectInput(session, "var_rep_audpc",   choices = c(opc_none, all))
  })

  # ---------------------------------------------------------------------------
  # ABA 1 — Outputs: Dados
  # ---------------------------------------------------------------------------

  output$n_linhas     <- renderText({ req(dados()); nrow(dados()) })
  output$n_colunas    <- renderText({ req(dados()); ncol(dados()) })
  output$tipo_arquivo <- renderText({
    req(input$fonte_dados)
    switch(input$fonte_dados,
           local   = if (!is.null(input$arquivo)) toupper(tools::file_ext(input$arquivo$name)) else "—",
           gsheets = "Google Sheets",
           exemplo = "Exemplo integrado",
           r_datasets = paste0("R Dataset: ", input$r_dataset_name))
  })

  output$tabela_preview <- renderDT({
    req(dados())
    datatable(dados(), options = list(pageLength = 8, scrollX = TRUE,
                                      dom = "lfrtip"),
              class = "table-striped table-hover table-sm",
              filter = "top", rownames = FALSE)
  })

  output$estrutura_dados <- renderPrint({
    req(dados())
    dplyr::glimpse(dados())
  })

  # ---------------------------------------------------------------------------
  # ABA 2 — Exploração
  # ---------------------------------------------------------------------------

  dados_transf <- reactive({
    req(dados(), input$var_resp_exp)
    req(input$var_resp_exp %in% names(dados()))  # guard contra coluna obsoleta
    df <- dados()
    y  <- df[[input$var_resp_exp]]
    y_t <- switch(input$transf_exp,
                  none  = y,
                  log   = log(y),
                  sqrt  = sqrt(y),
                  log1p = log1p(y))
    df[[paste0(input$var_resp_exp, "_transf")]] <- y_t
    df
  })

  output$grafico_explorar <- renderPlotly({
    req(dados_transf(), input$var_resp_exp)
    df  <- dados_transf()
    var_y <- if (input$transf_exp == "none") input$var_resp_exp else
      paste0(input$var_resp_exp, "_transf")
    label_y <- if (input$transf_exp == "none") input$var_resp_exp else
      paste0(input$transf_exp, "(", input$var_resp_exp, ")")

    usar_grupo <- !is.null(input$var_grupo_exp) && input$var_grupo_exp != ""

    p <- ggplot(df, aes(
      x     = if (usar_grupo) .data[[input$var_grupo_exp]] else factor("Todos"),
      y     = .data[[var_y]],
      color = if (usar_grupo) .data[[input$var_grupo_exp]] else NULL,
      fill  = if (usar_grupo) .data[[input$var_grupo_exp]] else NULL
    )) +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette  = "Set2") +
      labs(x = if (usar_grupo) input$var_grupo_exp else "",
           y = label_y, color = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank())

    if ("bar" %in% input$tipo_grafico)
      p <- p + stat_summary(fun = mean, geom = "col", alpha = 0.7, color = "white", position = position_dodge(width = 0.9))
    if ("violin" %in% input$tipo_grafico)
      p <- p + geom_violin(alpha = 0.25, width = 0.8)
    if ("box" %in% input$tipo_grafico)
      p <- p + geom_boxplot(outlier.colour = NA, alpha = 0.4, width = 0.6)
    if ("jitter" %in% input$tipo_grafico)
      p <- p + geom_jitter(width = 0.12, alpha = 0.7, size = 2.2)
    if ("mean" %in% input$tipo_grafico)
      p <- p + stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "black")
    if ("sd" %in% input$tipo_grafico)
      p <- p + stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2, linewidth = 0.8, color = "black")
    if ("se" %in% input$tipo_grafico)
      p <- p + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, linewidth = 0.8, color = "black")
    if ("tukey" %in% input$tipo_grafico && usar_grupo) {
      try({
        m <- aov(as.formula(paste(var_y, "~", input$var_grupo_exp)), data = df)
        em <- emmeans::emmeans(m, as.formula(paste("~", input$var_grupo_exp)))
        cld_res <- multcomp::cld(em, Letters = letters) |> as.data.frame()
        cld_res$.group <- trimws(cld_res$.group)
        
        # Pega o valor máximo de cada grupo para posicionar a letra
        df_max <- df |> group_by(.data[[input$var_grupo_exp]]) |> 
          summarise(max_val = max(.data[[var_y]], na.rm = TRUE), .groups = "drop")
        cld_res <- merge(cld_res, df_max, by = input$var_grupo_exp)
        
        y_nudge <- max(df[[var_y]], na.rm = TRUE) * 0.05
        p <- p + geom_text(data = cld_res, aes(x = .data[[input$var_grupo_exp]], y = max_val + y_nudge, label = .group), 
                           fontface = "bold", size = 5, color = "black", inherit.aes = FALSE)
      }, silent = TRUE)
    }

    ggplotly_wrapper(p, height = 550)
  })

  output$tabela_resumo <- renderDT({
    req(dados_transf(), input$var_resp_exp)
    req(input$var_resp_exp %in% names(dados_transf()))
    df <- dados_transf()
    var_y <- if (input$transf_exp == "none") input$var_resp_exp else paste0(input$var_resp_exp, "_transf")
    
    usar_grupo <- !is.null(input$var_grupo_exp) && input$var_grupo_exp != "" &&
                  input$var_grupo_exp %in% names(df)

    resumo <- if (usar_grupo) {
      df |>
        group_by(!!sym(input$var_grupo_exp)) |>
        summarise(
          n      = n(),
          Média  = round(mean(!!sym(var_y), na.rm = TRUE), 3),
          Mediana = round(median(!!sym(var_y), na.rm = TRUE), 3),
          DP     = round(sd(!!sym(var_y), na.rm = TRUE), 3),
          EP     = round(DP / sqrt(n), 3),
          Mín    = round(min(!!sym(var_y), na.rm = TRUE), 3),
          Máx    = round(max(!!sym(var_y), na.rm = TRUE), 3),
          `NA`   = sum(is.na(!!sym(var_y))),
          .groups = "drop"
        )
    } else {
      df |>
        summarise(
          n = n(),
          Média = round(mean(!!sym(var_y), na.rm = TRUE), 3),
          Mediana = round(median(!!sym(var_y), na.rm = TRUE), 3),
          DP  = round(sd(!!sym(var_y), na.rm = TRUE), 3),
          EP  = round(DP / sqrt(n), 3),
          Mín = round(min(!!sym(var_y), na.rm = TRUE), 3),
          Máx = round(max(!!sym(var_y), na.rm = TRUE), 3),
          `NA` = sum(is.na(!!sym(var_y)))
        )
    }

    datatable(resumo, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
              class = "table-striped table-sm", rownames = FALSE)
  })

  output$download_resumo <- downloadHandler(
    filename = function() paste0("resumo_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(dados(), input$var_resp_exp)
      df <- dados()
      usar_grupo <- !is.null(input$var_grupo_exp) && input$var_grupo_exp != ""
      resumo <- if (usar_grupo) {
        df |> group_by(.data[[input$var_grupo_exp]]) |>
          summarise(n = n(), media = mean(.data[[input$var_resp_exp]], na.rm = TRUE),
                    dp = sd(.data[[input$var_resp_exp]], na.rm = TRUE))
      } else {
        df |> summarise(n = n(), media = mean(.data[[input$var_resp_exp]], na.rm = TRUE),
                        dp = sd(.data[[input$var_resp_exp]], na.rm = TRUE))
      }
      write_csv(resumo, file)
    }
  )

  # ---------------------------------------------------------------------------
  # ABA 3 — Teste t
  # ---------------------------------------------------------------------------

  resultado_tt <- eventReactive(input$rodar_teste, {
    # --- Suporte a AUDPC ---
    usar_audpc <- isTRUE(input$usar_audpc_tt)
    if (usar_audpc) {
      res_audpc <- audpc_calculada()
      if (is.null(res_audpc) || !res_audpc$has_rep) {
        showNotification("⚠️ Calcule a AUDPC com repetição na Aba 7 antes de usar aqui.", type = "warning")
        return(NULL)
      }
      df    <- res_audpc$df_audpc
      var_y <- "audpc"
      var_g <- res_audpc$g_col
    } else {
      req(dados(), input$var_resp_tt, input$var_grupo_tt)
      df    <- dados()
      var_y <- input$var_resp_tt
      var_g <- input$var_grupo_tt
    }

    grupos <- unique(df[[var_g]])
    if (length(grupos) != 2) {
      showNotification("⚠️ Selecione um fator com exatamente 2 grupos.", type = "warning")
      return(NULL)
    }

    # Aplicar transformação
    y_original <- df[[var_y]]
    transf <- aplicar_transformacao(y_original, input$transf_tt)
    var_y_transf <- paste0(var_y, "_T")
    df[[var_y_transf]] <- transf$y

    tryCatch({
      if (input$tipo_teste == "t_indep") {
        res <- t.test(as.formula(paste(var_y_transf, "~", var_g)), data = df,
                      var.equal = input$var_equal)
      } else if (input$tipo_teste == "t_pareado") {
        g1 <- df[[var_y_transf]][df[[var_g]] == grupos[1]]
        g2 <- df[[var_y_transf]][df[[var_g]] == grupos[2]]
        res <- t.test(g1, g2, paired = TRUE)
      } else {
        res <- wilcox.test(as.formula(paste(var_y_transf, "~", var_g)), data = df)
      }
      list(resultado = res, grupos = grupos, df = df, var_y = var_y_transf, var_g = var_g,
           var_y_orig = var_y, y_original = y_original, y_transf = transf$y,
           transf_label = transf$label, transf_tipo = input$transf_tt)
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error"); NULL
    })
  })


  output$resultado_cards_tt <- renderUI({
    res_list <- resultado_tt()
    req(res_list)
    res <- res_list$resultado
    pval <- res$p.value
    sig  <- if (!is.na(pval) && pval < input$alpha_tt) "✅ Significativo" else "❌ Não significativo"
    cor  <- if (!is.na(pval) && pval < input$alpha_tt) "#2C7A4B" else "#e74c3c"

    tagList(
      stat_card("p-valor", formatC(pval, digits = 4, format = "g"), "📊", cor),
      if (!is.null(res$statistic))
        stat_card("Estatística do teste",
                  paste0(names(res$statistic), " = ",
                         round(res$statistic, 3)), "📐", "#2980b9"),
      if (!is.null(res$parameter))
        stat_card("Graus de liberdade", round(res$parameter, 1), "🔢", "#8e44ad"),
      stat_card("Conclusão (α = 0.05)", sig, "🎯", cor)
    )
  })

  output$resultado_raw_tt <- renderPrint({
    res_list <- resultado_tt()
    req(res_list)
    print(res_list$resultado)
  })

  output$grafico_distribuicao_t <- renderPlotly({
    res_list <- resultado_tt()
    req(res_list)
    res <- res_list$resultado

    if (input$tipo_teste == "wilcoxon") {
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, size = 5,
                 label = paste0("Wilcoxon W = ", res$statistic,
                                "\np-valor = ", formatC(res$p.value, digits = 4, format = "g"))) +
        theme_void()
      return(ggplotly_wrapper(p))
    }

    t_obs <- as.numeric(res$statistic)
    gl    <- if (!is.null(res$parameter)) as.numeric(res$parameter) else 30
    lim_x <- max(4, abs(t_obs) + 1.5)

    x_seq <- seq(-lim_x, lim_x, length.out = 500)
    y_seq <- dt(x_seq, df = gl)
    df_curve <- data.frame(x = x_seq, y = y_seq)

    # Área de rejeição
    x_rej_left  <- x_seq[x_seq < -abs(t_obs)]
    x_rej_right <- x_seq[x_seq > abs(t_obs)]

    p <- ggplot(df_curve, aes(x, y)) +
      geom_line(color = "#2C7A4B", linewidth = 1.1) +
      geom_area(data = data.frame(
        x = c(-lim_x, x_rej_left, -abs(t_obs)),
        y = c(0, dt(x_rej_left, gl), 0)),
        aes(x, y), fill = "#e74c3c", alpha = 0.35) +
      geom_area(data = data.frame(
        x = c(abs(t_obs), x_rej_right, lim_x),
        y = c(0, dt(x_rej_right, gl), 0)),
        aes(x, y), fill = "#e74c3c", alpha = 0.35) +
      geom_vline(xintercept = c(-abs(t_obs), abs(t_obs)),
                 color = "#e74c3c", linetype = "dashed", linewidth = 0.9) +
      annotate("text", x = 0, y = max(y_seq) * 0.85, size = 3.5,
               label = paste0("t = ", round(t_obs, 3), " | GL = ", round(gl, 1),
                              "\np = ", formatC(res$p.value, digits = 4, format = "g"))) +
      labs(x = "Valores de t", y = "Densidade",
           title = "Distribuição t de Student sob H₀") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())

    ggplotly_wrapper(p)
  })

  output$grafico_tt_grupos <- renderPlotly({
    res_list <- resultado_tt()
    req(res_list)
    df    <- res_list$df
    var_y <- res_list$var_y
    var_g <- res_list$var_g

    p <- ggplot(df, aes(x = .data[[var_g]], y = .data[[var_y]],
                        color = .data[[var_g]])) +
      geom_boxplot(outlier.colour = NA, width = 0.5, alpha = 0.3) +
      geom_jitter(width = 0.12, alpha = 0.7, size = 2.5) +
      stat_summary(fun = mean, geom = "point", shape = 18,
                   size = 5, color = "black") +
      scale_color_brewer(palette = "Set2") +
      labs(x = var_g, y = var_y, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")

    ggplotly_wrapper(p)
  })

  output$shapiro_tt <- renderPrint({
    req(dados(), input$var_resp_tt)
    y <- dados()[[input$var_resp_tt]]
    y <- y[!is.na(y)]
    if (length(y) < 3 || length(y) > 5000) {
      cat("Shapiro-Wilk requer entre 3 e 5000 observações.\n")
    } else {
      print(shapiro.test(y))
    }
  })

  output$homog_tt <- renderPrint({
    req(dados(), input$var_resp_tt, input$var_grupo_tt)
    df <- dados()
    tryCatch({
      cat("=== Teste de Bartlett ===\n")
      print(bartlett.test(as.formula(paste(input$var_resp_tt, "~",
                                           input$var_grupo_tt)), data = df))
      cat("\n=== Teste de Levene (car) ===\n")
      print(car::leveneTest(as.formula(paste(input$var_resp_tt, "~",
                                             input$var_grupo_tt)),
                            data = df))
    }, error = function(e) cat("Erro:", e$message, "\n"))
  })

  output$qqplot_tt <- renderPlotly({
    req(dados(), input$var_resp_tt)
    y <- dados()[[input$var_resp_tt]]
    df_qq <- data.frame(
      sample = sort(y, na.last = NA),
      theoretical = qnorm(ppoints(sum(!is.na(y))))
    )
    p <- ggplot(df_qq, aes(theoretical, sample)) +
      geom_point(color = "#2C7A4B", alpha = 0.7, size = 2) +
      geom_qq_line(aes(sample = sample), color = "red", linetype = "dashed") +
      labs(x = "Quantis teóricos", y = "Quantis amostrais",
           title = "QQ-Plot de Normalidade") +
      theme_minimal(base_size = 12)
    ggplotly_wrapper(p, height = 280)
  })

  # Histograma de comparação: original vs transformado (teste t)
  output$hist_transf_tt <- renderPlotly({
    res <- resultado_tt()
    req(res)
    if (res$transf_tipo == "none") {
      p <- ggplot(data.frame(y = res$y_original), aes(x = y)) +
        geom_histogram(fill = "#2C7A4B", color = "white", bins = 15, alpha = 0.8) +
        labs(title = paste("Distribuição original:", res$var_y_orig), x = res$var_y_orig, y = "Frequência") +
        theme_minimal(base_size = 12)
      return(ggplotly_wrapper(p, height = 380))
    }
    df_comp <- data.frame(
      valor = c(res$y_original, res$y_transf),
      tipo  = rep(c(paste0("Original (", res$var_y_orig, ")"),
                     paste0("Transformado: ", res$transf_label)),
                   each = length(res$y_original))
    )
    p <- ggplot(df_comp, aes(x = valor, fill = tipo)) +
      geom_histogram(color = "white", bins = 15, alpha = 0.8) +
      facet_wrap(~ tipo, scales = "free") +
      scale_fill_manual(values = c("#95a5a6", "#2C7A4B")) +
      labs(title = "Comparação: Original vs Transformado", x = "Valor", y = "Frequência") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", strip.text = element_text(face = "bold"))
    ggplotly_wrapper(p, height = 380)
  })

  output$resumo_transf_tt <- renderPrint({
    res <- resultado_tt()
    req(res)
    cat("=====================================================\n")
    cat(" TRANSFORMAÇÃO APLICADA:", res$transf_label, "\n")
    cat("=====================================================\n\n")
    cat("--- Dados Originais ---\n")
    cat("  n     =", length(na.omit(res$y_original)), "\n")
    cat("  Média =", round(mean(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  DP    =", round(sd(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  Mín   =", round(min(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  Máx   =", round(max(res$y_original, na.rm = TRUE), 4), "\n\n")

    if (res$transf_tipo != "none") {
      cat("--- Dados Transformados ---\n")
      cat("  n     =", length(na.omit(res$y_transf)), "\n")
      cat("  Média =", round(mean(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  DP    =", round(sd(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  Mín   =", round(min(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  Máx   =", round(max(res$y_transf, na.rm = TRUE), 4), "\n\n")
      cat("--- Shapiro-Wilk (transformados) ---\n")
      y_t <- na.omit(res$y_transf)
      if (length(y_t) >= 3 && length(y_t) <= 5000) {
        print(shapiro.test(y_t))
      } else {
        cat("Shapiro-Wilk requer entre 3 e 5000 observações.\n")
      }
    } else {
      cat("Nenhuma transformação foi aplicada.\n")
    }
  })

  # Relatório Textual - Teste t
  output$report_tt <- renderPrint({
    res <- resultado_tt()
    req(res)
    cat("Gerando relatório (isso pode demorar alguns segundos)...\n\n")
    tryCatch({
      print(report::report(res$resultado))
    }, error = function(e) cat("Erro ao gerar relatório:", e$message))
  })

  output$download_report_tt <- downloadHandler(
    filename = function() { paste0("relatorio_testet_", Sys.Date(), ".txt") },
    content = function(file) {
      res <- resultado_tt()
      req(res)
      texto <- tryCatch(as.character(report::report(res$resultado)), error = function(e) "Erro ao gerar relatório.")
      writeLines(texto, file)
    }
  )

  # ---------------------------------------------------------------------------
  # ABA 4 — ANOVA
  # ---------------------------------------------------------------------------

  resultado_anova <- eventReactive(input$rodar_anova, {
    # --- Suporte a AUDPC ---
    usar_audpc <- isTRUE(input$usar_audpc_av)
    if (usar_audpc) {
      res_audpc <- audpc_calculada()
      if (is.null(res_audpc) || !res_audpc$has_rep) {
        showNotification("⚠️ Calcule a AUDPC com repetição na Aba 7 antes de usar aqui.", type = "warning")
        return(NULL)
      }
      df    <- res_audpc$df_audpc
      var_y <- "audpc"
      var_t <- res_audpc$g_col
      df[[var_t]] <- as.factor(df[[var_t]])
      transf <- aplicar_transformacao(df[[var_y]], input$transf_av)
      y_orig <- df[[var_y]]
      df[[var_y]] <- transf$y
      formula_str <- paste(var_y, "~", var_t)
      tryCatch({
        m   <- aov(as.formula(formula_str), data = df)
        tbl <- as.data.frame(anova(m))
        em  <- emmeans(m, as.formula(paste("~", var_t)))
        cld_res <- multcomp::cld(em, Letters = letters) |> as.data.frame()
        return(list(modelo = m, tabela = tbl, emmeans = em, cld = cld_res,
                    df = df, var_y = var_y, var_t = var_t,
                    y_original = y_orig, y_transf = transf$y,
                    transf_label = transf$label, transf_tipo = input$transf_av))
      }, error = function(e) {
        showNotification(paste("Erro ANOVA AUDPC:", e$message), type = "error"); NULL
      })
    }

    if (input$delineamento == "fatorial") {

      req(dados(), input$var_resp_av, input$fator_principal, input$fator_desdobramento)
      df    <- dados()
      var_y <- input$var_resp_av
      f_principal <- input$fator_principal
      f_desdobramento <- input$fator_desdobramento
      df[[f_principal]] <- as.factor(df[[f_principal]])
      df[[f_desdobramento]] <- as.factor(df[[f_desdobramento]])
      # Aplicar transformação
      transf <- aplicar_transformacao(df[[var_y]], input$transf_av,
                                      as.formula(paste(var_y, "~", f_principal, "*", f_desdobramento)), df)
      y_orig <- df[[var_y]]
      df[[var_y]] <- transf$y
      formula_str <- paste(var_y, "~", f_principal, "*", f_desdobramento)
    } else if (input$delineamento == "splitplot") {
      req(dados(), input$var_resp_av, input$split_fator_a, input$split_fator_b, input$split_bloco)
      df    <- dados()
      var_y <- input$var_resp_av
      split_fator_a <- input$split_fator_a
      split_fator_b <- input$split_fator_b
      split_bloco   <- input$split_bloco
      df[[split_fator_a]] <- as.factor(df[[split_fator_a]])
      df[[split_fator_b]] <- as.factor(df[[split_fator_b]])
      df[[split_bloco]]   <- as.factor(df[[split_bloco]])
      # Aplicar transformação
      transf <- aplicar_transformacao(df[[var_y]], input$transf_av)
      y_orig <- df[[var_y]]
      df[[var_y]] <- transf$y
      formula_str <- paste(var_y, "~", split_bloco, "+", split_fator_a, "*", split_fator_b, "+ (1 |", split_bloco, ":", split_fator_a, ")")
    } else if (input$delineamento == "misto") {
      req(dados(), input$var_resp_av, input$misto_fator_a, input$misto_fator_b, input$misto_bloco)
      df    <- dados()
      var_y <- input$var_resp_av
      misto_fator_a <- input$misto_fator_a
      misto_fator_b <- input$misto_fator_b
      misto_bloco   <- input$misto_bloco
      df[[misto_fator_a]] <- as.factor(df[[misto_fator_a]])
      df[[misto_fator_b]] <- as.factor(df[[misto_fator_b]])
      df[[misto_bloco]]   <- as.factor(df[[misto_bloco]])
      # Aplicar transformação
      transf <- aplicar_transformacao(df[[var_y]], input$transf_av)
      y_orig <- df[[var_y]]
      df[[var_y]] <- transf$y
      # Fórmula correta para parcelas subdivididas via LMM:
      # Y ~ FatorA * FatorB + (1|Bloco) + (1|Bloco:FatorA)
      formula_str <- paste0(var_y, " ~ ", misto_fator_a, " * ", misto_fator_b,
                            " + (1 | ", misto_bloco, ") + (1 | ", misto_bloco, ":", misto_fator_a, ")")    
    } else {
      req(dados(), input$var_resp_av, input$var_trat_av)
      df    <- dados()
      var_y <- input$var_resp_av
      var_t <- input$var_trat_av
      df[[var_t]] <- as.factor(df[[var_t]])
      # Aplicar transformação
      transf <- aplicar_transformacao(df[[var_y]], input$transf_av,
                                      as.formula(paste(var_y, "~", var_t)), df)
      y_orig <- df[[var_y]]
      df[[var_y]] <- transf$y
      
      formula_str <- switch(input$delineamento,
        dic     = paste(var_y, "~", var_t),
        dbc     = { req(input$var_bloco_av); df[[input$var_bloco_av]] <- as.factor(df[[input$var_bloco_av]])
                    paste(var_y, "~", var_t, "+", input$var_bloco_av) }
      )
    }

    tryCatch({
      if (input$delineamento == "fatorial") {
        m     <- aov(as.formula(formula_str), data = df)
        tbl   <- as.data.frame(anova(m))
        
        # specs formula com A | B ou B | A dependendo do sentido do desdobramento
        specs_formula <- if (input$desdobrar_sentido == "normal") {
          formula(paste("~", f_principal, "|", f_desdobramento))
        } else {
          formula(paste("~", f_desdobramento, "|", f_principal))
        }
        
        em    <- emmeans(m, specs = specs_formula)
        cld_res <- multcomp::cld(em, Letters = letters) |> as.data.frame()
        
        list(modelo = m, tabela = tbl, emmeans = em, cld = cld_res,
             df = df, var_y = var_y, var_t = f_principal, f_principal = f_principal, f_desdobramento = f_desdobramento,
             y_original = y_orig, y_transf = transf$y, transf_label = transf$label, transf_tipo = input$transf_av)
      } else if (input$delineamento == "splitplot") {
        library(lmerTest)
        m     <- lmerTest::lmer(as.formula(formula_str), data = df)
        tbl   <- as.data.frame(anova(m))
        
        # specs formula com A | B ou B | A dependendo do sentido do desdobramento
        specs_formula <- if (input$desdobrar_sentido == "normal") {
          formula(paste("~", split_fator_a, "|", split_fator_b))
        } else {
          formula(paste("~", split_fator_b, "|", split_fator_a))
        }
        
        em    <- emmeans(m, specs = specs_formula)
        cld_res <- multcomp::cld(em, Letters = letters) |> as.data.frame()
        
        list(modelo = m, tabela = tbl, emmeans = em, cld = cld_res,
             df = df, var_y = var_y, var_t = split_fator_a, f_principal = split_fator_a, f_desdobramento = split_fator_b,
             y_original = y_orig, y_transf = transf$y, transf_label = transf$label, transf_tipo = input$transf_av)
      } else if (input$delineamento == "misto") {
        library(lmerTest)
        m   <- lmerTest::lmer(as.formula(formula_str), data = df)
        tbl <- as.data.frame(anova(m))
        # Renomear colunas lmerTest para nomes canônicos usados no renderDT
        if ("Pr(>F)" %in% colnames(tbl) && !"Pr.F." %in% colnames(tbl)) {
          # já está correto
        }
        # emmeans da interação FatorA * FatorB (igual ao curso)
        em_interacao <- emmeans(m, as.formula(paste("~", misto_fator_a, "*", misto_fator_b)))
        # letras para FatorA dentro de FatorB
        em_a_por_b   <- emmeans(m, as.formula(paste("~", misto_fator_a, "|", misto_fator_b)))
        # letras para FatorB dentro de FatorA
        em_b_por_a   <- emmeans(m, as.formula(paste("~", misto_fator_b, "|", misto_fator_a)))

        cld_a <- tryCatch(
          multcomp::cld(em_a_por_b, Letters = letters,  adjust = "sidak") |> as.data.frame(),
          error = function(e) NULL
        )
        cld_b <- tryCatch(
          multcomp::cld(em_b_por_a, Letters = LETTERS, adjust = "sidak") |> as.data.frame(),
          error = function(e) NULL
        )

        # Tabela final combinada
        tbl_em <- as.data.frame(em_interacao)
        if (!is.null(cld_a) && !is.null(cld_b)) {
          cld_a_sel <- cld_a |> dplyr::select(dplyr::all_of(c(misto_fator_a, misto_fator_b)), letra_a = .group)
          cld_a_sel$letra_a <- trimws(cld_a_sel$letra_a)
          cld_b_sel <- cld_b |> dplyr::select(dplyr::all_of(c(misto_fator_a, misto_fator_b)), letra_b = .group)
          cld_b_sel$letra_b <- trimws(cld_b_sel$letra_b)
          tbl_em <- dplyr::left_join(tbl_em, cld_a_sel, by = c(misto_fator_a, misto_fator_b))
          tbl_em <- dplyr::left_join(tbl_em, cld_b_sel, by = c(misto_fator_a, misto_fator_b))
          tbl_em$.group <- paste0(tbl_em$letra_a, " ", tbl_em$letra_b)
        } else {
          tbl_em$.group <- ""
        }

        list(modelo = m, tabela = tbl, emmeans = em_interacao, cld = tbl_em,
             df = df, var_y = var_y, var_t = misto_fator_a,
             f_principal = misto_fator_a, f_desdobramento = misto_fator_b,
             y_original = y_orig, y_transf = transf$y, transf_label = transf$label, transf_tipo = input$transf_av)
      } else {
        m     <- lm(as.formula(formula_str), data = df)
        tbl   <- as.data.frame(anova(m))
        em    <- emmeans(m, as.formula(paste("~", var_t)))
        cld_res <- multcomp::cld(em, Letters = letters,
                                 adjust = input$metodo_cld) |> as.data.frame()
        list(modelo = m, tabela = tbl, emmeans = em, cld = cld_res,
             df = df, var_y = var_y, var_t = var_t,
             y_original = y_orig, y_transf = transf$y, transf_label = transf$label, transf_tipo = input$transf_av)
      }
    }, error = function(e) {
      showNotification(paste("Erro ANOVA:", e$message), type = "error"); NULL
    })
  })

  output$tabela_anova <- renderDT({
    res <- resultado_anova()
    req(res)
    tbl <- res$tabela

    # Detectar a coluna de p-valor (difere entre anova() clássica e lmerTest)
    p_col <- if ("Pr(>F)" %in% colnames(tbl)) "Pr(>F)" else NULL

    # Arredondar apenas colunas numéricas
    tbl <- tbl |> mutate(across(where(is.numeric), ~round(., 4)))

    if (!is.null(p_col)) {
      tbl[["p-valor"]] <- formatC(as.numeric(tbl[[p_col]]), digits = 4, format = "g")
    }

    dt <- datatable(tbl, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
                    class = "table-striped table-sm")

    if (!is.null(p_col) && p_col %in% colnames(tbl)) {
      dt <- dt |> formatStyle(p_col, backgroundColor = styleInterval(0.05, c("#ffeaea", "white")))
    }
    dt
  })

  output$grafico_emmeans <- renderPlotly({
    res <- resultado_anova()
    req(res)
    cld <- res$cld

    # Limpar espaços das letras
    cld$.group <- trimws(cld$.group)

    # Obter os nomes das colunas de intervalo de confiança de forma segura
    ci_lower <- if ("lower.CL" %in% colnames(cld)) "lower.CL" else (if ("asymp.LCL" %in% colnames(cld)) "asymp.LCL" else grep("LCL|lower", colnames(cld), value = TRUE)[1])
    ci_upper <- if ("upper.CL" %in% colnames(cld)) "upper.CL" else (if ("asymp.UCL" %in% colnames(cld)) "asymp.UCL" else grep("UCL|upper", colnames(cld), value = TRUE)[1])
    
    if (!is.null(ci_lower) && !is.na(ci_lower) && ci_lower %in% colnames(cld) && ci_lower != "lower.CL") cld$lower.CL <- cld[[ci_lower]]
    if (!is.null(ci_upper) && !is.na(ci_upper) && ci_upper %in% colnames(cld) && ci_upper != "upper.CL") cld$upper.CL <- cld[[ci_upper]]

    if (input$delineamento == "misto") {
      # Gráfico de barras com médias ajustadas por combinação FatorA * FatorB (estilo do curso)
      f_a <- res$f_principal
      f_b <- res$f_desdobramento
      cld$ymax_bar <- cld$emmean + cld$SE

      p <- ggplot(cld, aes(x = .data[[f_a]], y = emmean, fill = .data[[f_b]])) +
        geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
        geom_errorbar(
          aes(ymin = emmean - SE, ymax = emmean + SE),
          position = position_dodge(width = 0.8), width = 0.2
        ) +
        geom_text(
          aes(label = .group, y = ymax_bar + max(cld$emmean, na.rm = TRUE) * 0.04),
          position = position_dodge(width = 0.8),
          size = 3.5, fontface = "bold"
        ) +
        scale_fill_brewer(palette = "Set2") +
        labs(
          x = f_a, y = paste0("Média ajustada de ", res$var_y),
          fill = f_b,
          caption = "Letras minúsculas: híbridos dentro de método; maiúsculas: métodos dentro de híbrido"
        ) +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "top", panel.grid.minor = element_blank())
    } else if (input$delineamento %in% c("fatorial", "splitplot")) {
      x_var <- if (input$desdobrar_sentido == "normal") res$f_principal else res$f_desdobramento
      facet_var <- if (input$desdobrar_sentido == "normal") res$f_desdobramento else res$f_principal
      
      p <- ggplot(cld, aes(x = reorder(.data[[x_var]], emmean), y = emmean, color = .data[[x_var]], fill = .data[[x_var]])) +
        geom_point(size = 3.5) +
        geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, linewidth = 0.8) +
        geom_text(aes(label = .group, y = upper.CL), vjust = -0.6, fontface = "bold", size = 4) +
        facet_wrap(vars(.data[[facet_var]]), labeller = label_both) +
        coord_flip() +
        scale_color_brewer(palette = "Set2") +
        scale_fill_brewer(palette = "Set2") +
        labs(x = x_var, y = paste0("Média ajustada de ", res$var_y, " (IC 95%)")) +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(), legend.position = "none")
    } else {
      p <- ggplot(cld, aes(x = reorder(.data[[res$var_t]], emmean), y = emmean)) +
        geom_point(size = 3.5, color = "#2C7A4B") +
        geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                      width = 0.15, color = "#2C7A4B", linewidth = 0.8) +
        geom_text(aes(label = .group, y = upper.CL),
                  vjust = -0.6, fontface = "bold", size = 4, color = "#2C7A4B") +
        coord_flip() +
        labs(x = res$var_t,
             y = paste0("Média ajustada de ", res$var_y, " (IC 95%)"),
             caption = "Letras iguais = não diferem significativamente (Tukey, α = 0.05)") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank())
    }

    ggplotly_wrapper(p)
  })

  output$tabela_emmeans <- renderDT({
    res <- resultado_anova()
    req(res)
    tbl <- as.data.frame(res$cld)
    
    # Raciocínio de colunas seguro para intervalos de confiança
    actual_cols <- colnames(tbl)
    ci_lower <- if ("lower.CL" %in% actual_cols) "lower.CL" else (if ("asymp.LCL" %in% actual_cols) "asymp.LCL" else grep("LCL|lower", actual_cols, value = TRUE)[1])
    ci_upper <- if ("upper.CL" %in% actual_cols) "upper.CL" else (if ("asymp.UCL" %in% actual_cols) "asymp.UCL" else grep("UCL|upper", actual_cols, value = TRUE)[1])
    
    if (!is.null(ci_lower) && !is.na(ci_lower) && ci_lower %in% actual_cols && ci_lower != "lower.CL") {
      colnames(tbl)[colnames(tbl) == ci_lower] <- "lower.CL"
    }
    if (!is.null(ci_upper) && !is.na(ci_upper) && ci_upper %in% actual_cols && ci_upper != "upper.CL") {
      colnames(tbl)[colnames(tbl) == ci_upper] <- "upper.CL"
    }

    if (input$delineamento == "misto") {
      # Tabela de médias da interação FatorA * FatorB
      cols_to_keep <- c(res$f_principal, res$f_desdobramento, "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")
      cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(tbl)]
      tbl <- tbl[, cols_to_keep, drop = FALSE]
    } else if (input$delineamento %in% c("fatorial", "splitplot")) {
      # Selecionar e ordenar as colunas: fatores, emmean, SE, df, lower.CL, upper.CL, .group
      cols_to_keep <- c(res$f_principal, res$f_desdobramento, "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")
      cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(tbl)]
      tbl <- tbl[, cols_to_keep, drop = FALSE]
    } else {
      cols_to_keep <- c(res$var_t, "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")
      cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(tbl)]
      tbl <- tbl[, cols_to_keep, drop = FALSE]
    }
    
    tbl <- tbl |> mutate(across(where(is.numeric), ~round(., 3)))
    datatable(tbl, options = list(dom = "t", paging = FALSE, scrollX = TRUE),
              class = "table-striped table-sm", rownames = FALSE)
  })

  output$plot_dharma_av <- renderPlot({
    res <- resultado_anova()
    req(res)
    # Gera os resíduos simulados do pacote DHARMa
    sim_res <- DHARMa::simulateResiduals(fittedModel = res$modelo, plot = FALSE)
    plot(sim_res)
  })

  output$testes_dharma_av <- renderPrint({
    res <- resultado_anova()
    req(res)
    sim_res <- DHARMa::simulateResiduals(fittedModel = res$modelo, plot = FALSE)
    
    cat("========================================================\n")
    cat(" TESTES DE PREMISSAS VIA DHARMa (Resíduos Simulados)\n")
    cat("========================================================\n\n")
    
    cat("1. TESTE DE UNIFORMIDADE (Kolmogorov-Smirnov)\n")
    cat("Verifica se a distribuição geral dos resíduos está correta.\n")
    print(DHARMa::testUniformity(sim_res))
    
    cat("\n2. TESTE DE DISPERSÃO\n")
    cat("Verifica se há super ou sub-dispersão.\n")
    print(DHARMa::testDispersion(sim_res))
    
    cat("\n3. TESTE DE OUTLIERS\n")
    cat("Verifica se existem mais outliers do que o esperado.\n")
    print(DHARMa::testOutliers(sim_res))
  })

  # Histograma de comparação: original vs transformado (ANOVA)
  output$hist_transf_av <- renderPlotly({
    res <- resultado_anova()
    req(res)
    if (res$transf_tipo == "none") {
      p <- ggplot(data.frame(y = res$y_original), aes(x = y)) +
        geom_histogram(fill = "#2C7A4B", color = "white", bins = 15, alpha = 0.8) +
        labs(title = paste("Distribuição original:", res$var_y), x = res$var_y, y = "Frequência") +
        theme_minimal(base_size = 12)
      return(ggplotly_wrapper(p, height = 380))
    }
    df_comp <- data.frame(
      valor = c(res$y_original, res$y_transf),
      tipo  = rep(c(paste0("Original (", res$var_y, ")"),
                     paste0("Transformado: ", res$transf_label)),
                   each = length(res$y_original))
    )
    p <- ggplot(df_comp, aes(x = valor, fill = tipo)) +
      geom_histogram(color = "white", bins = 15, alpha = 0.8) +
      facet_wrap(~ tipo, scales = "free") +
      scale_fill_manual(values = c("#95a5a6", "#2C7A4B")) +
      labs(title = "Comparação: Original vs Transformado", x = "Valor", y = "Frequência") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none", strip.text = element_text(face = "bold"))
    ggplotly_wrapper(p, height = 380)
  })

  output$resumo_transf_av <- renderPrint({
    res <- resultado_anova()
    req(res)
    cat("=====================================================\n")
    cat(" TRANSFORMAÇÃO APLICADA:", res$transf_label, "\n")
    cat("=====================================================\n\n")
    cat("--- Dados Originais ---\n")
    cat("  n     =", length(na.omit(res$y_original)), "\n")
    cat("  Média =", round(mean(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  DP    =", round(sd(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  Mín   =", round(min(res$y_original, na.rm = TRUE), 4), "\n")
    cat("  Máx   =", round(max(res$y_original, na.rm = TRUE), 4), "\n\n")

    if (res$transf_tipo != "none") {
      cat("--- Dados Transformados ---\n")
      cat("  n     =", length(na.omit(res$y_transf)), "\n")
      cat("  Média =", round(mean(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  DP    =", round(sd(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  Mín   =", round(min(res$y_transf, na.rm = TRUE), 4), "\n")
      cat("  Máx   =", round(max(res$y_transf, na.rm = TRUE), 4), "\n\n")
      cat("--- Shapiro-Wilk (transformados) ---\n")
      y_t <- na.omit(res$y_transf)
      if (length(y_t) >= 3 && length(y_t) <= 5000) {
        print(shapiro.test(y_t))
      } else {
        cat("Shapiro-Wilk requer entre 3 e 5000 observações.\n")
      }
    } else {
      cat("Nenhuma transformação foi aplicada.\n")
    }
  })

  # Relatório Textual - ANOVA
  output$report_av <- renderPrint({
    res <- resultado_anova()
    req(res)
    cat("Gerando relatório descritivo (isso pode demorar alguns segundos)...\n\n")
    tryCatch({
      print(report::report(res$modelo))
    }, error = function(e) cat("Erro ao gerar relatório:", e$message))
  })

  output$download_report_av <- downloadHandler(
    filename = function() { paste0("relatorio_anova_", Sys.Date(), ".txt") },
    content = function(file) {
      res <- resultado_anova()
      req(res)
      texto <- tryCatch(as.character(report::report(res$modelo)), error = function(e) "Erro ao gerar relatório.")
      writeLines(texto, file)
    }
  )

  # ---------------------------------------------------------------------------
  # ABA 5 — Regressão e Correlação
  # ---------------------------------------------------------------------------

  resultado_reg <- eventReactive(input$rodar_reg, {
    req(dados(), input$var_x_reg, input$var_y_reg)
    df  <- dados()
    x   <- input$var_x_reg
    y   <- input$var_y_reg
    tryCatch({
      if (input$tipo_reg %in% c("pearson", "spearman")) {
        res <- cor.test(df[[x]], df[[y]], method = input$tipo_reg)
        list(tipo = "cor", res = res, df = df, x = x, y = y)
      } else {
        formula_str <- switch(input$tipo_reg,
          linear = paste(y, "~", x),
          poly2  = paste(y, "~ poly(", x, ", 2, raw = TRUE)"),
          poly3  = paste(y, "~ poly(", x, ", 3, raw = TRUE)")
        )
        m   <- lm(as.formula(formula_str), data = df)
        res_tidy <- tidy(m)
        res_glance <- glance(m)
        list(tipo = "reg", modelo = m, tidy = res_tidy, glance = res_glance,
             df = df, x = x, y = y)
      }
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error"); NULL
    })
  })

  output$grafico_reg <- renderPlotly({
    res_list <- resultado_reg()
    req(res_list)
    df <- res_list$df
    x  <- res_list$x
    y  <- res_list$y

    usar_cor <- !is.null(input$var_cor_reg) && input$var_cor_reg != ""

    p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]],
                        color = if (usar_cor) .data[[input$var_cor_reg]] else NULL)) +
      geom_point(alpha = 0.7, size = 2.5) +
      scale_color_brewer(palette = "Set2") +
      labs(x = x, y = y, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")

    metodo_smooth <- switch(input$tipo_reg,
      linear = "lm",
      poly2  = "lm",
      poly3  = "lm",
      "lm"
    )
    formula_smooth <- switch(input$tipo_reg,
      poly2 = y ~ poly(x, 2),
      poly3 = y ~ poly(x, 3),
      y ~ x
    )

    if (input$tipo_reg %in% c("pearson", "spearman")) {
      p <- p + geom_smooth(method = "lm", se = input$mostrar_ic_reg,
                           color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
    } else {
      p <- p + geom_smooth(method = metodo_smooth, formula = formula_smooth,
                           se = input$mostrar_ic_reg,
                           color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
    }

    if (input$mostrar_eq_reg && res_list$tipo == "reg") {
      m     <- res_list$modelo
      coefs <- coef(m)
      r2    <- round(summary(m)$r.squared, 3)
      eq_lab <- paste0("R² = ", r2)
      p <- p + annotate("text", x = min(df[[x]], na.rm = TRUE),
                        y = max(df[[y]], na.rm = TRUE),
                        label = eq_lab, hjust = 0, vjust = 1,
                        size = 4, color = "#2C7A4B", fontface = "italic")
    }

    ggplotly_wrapper(p)
  })

  output$resultado_reg_cards <- renderUI({
    res_list <- resultado_reg()
    req(res_list)
    if (res_list$tipo == "cor") {
      res <- res_list$res
      tagList(
        stat_card("Coeficiente r", round(res$estimate, 4), "📊"),
        stat_card("p-valor", formatC(res$p.value, digits = 4, format = "g"), "📈",
                  if (res$p.value < 0.05) "#2C7A4B" else "#e74c3c"),
        stat_card("IC 95%",
                  paste0("[", round(res$conf.int[1], 3), "; ", round(res$conf.int[2], 3), "]"), "📐")
      )
    } else {
      gl <- res_list$glance
      tagList(
        stat_card("R²",         round(gl$r.squared, 4), "📊"),
        stat_card("R² ajustado", round(gl$adj.r.squared, 4), "📈"),
        stat_card("F-estatística", round(gl$statistic, 3), "📐"),
        stat_card("p-valor (modelo)", formatC(gl$p.value, digits = 4, format = "g"), "🎯",
                  if (gl$p.value < 0.05) "#2C7A4B" else "#e74c3c")
      )
    }
  })

  output$resultado_reg_raw <- renderPrint({
    res_list <- resultado_reg()
    req(res_list)
    if (res_list$tipo == "cor") {
      print(res_list$res)
    } else {
      print(summary(res_list$modelo))
    }
  })

  output$ponto_otimo <- renderUI({
    req(resultado_reg(), input$tipo_reg == "poly2")
    res_list <- resultado_reg()
    req(res_list$tipo == "reg")
    coefs <- coef(res_list$modelo)
    if (length(coefs) < 3) return(NULL)
    a <- coefs[3]; b <- coefs[2]
    x_otimo <- round(-b / (2 * a), 3)
    y_otimo  <- round(coefs[1] + b * x_otimo + a * x_otimo^2, 3)
    tipo_ponto <- if (a < 0) "Máximo" else "Mínimo"
    tagList(
      stat_card(paste("Ponto de", tipo_ponto, "— X"), x_otimo, "🎯"),
      stat_card(paste("Ponto de", tipo_ponto, "— Y"), y_otimo, "📈")
    )
  })

  # Relatório Textual - Regressão
  output$report_reg <- renderPrint({
    res_list <- resultado_reg()
    req(res_list)
    cat("Gerando relatório descritivo (isso pode demorar alguns segundos)...\n\n")
    tryCatch({
      if (res_list$tipo == "cor") {
        print(report::report(res_list$res))
      } else {
        print(report::report(res_list$modelo))
      }
    }, error = function(e) cat("Erro ao gerar relatório:", e$message))
  })

  output$download_report_reg <- downloadHandler(
    filename = function() { paste0("relatorio_regressao_", Sys.Date(), ".txt") },
    content = function(file) {
      res_list <- resultado_reg()
      req(res_list)
      texto <- tryCatch({
        if (res_list$tipo == "cor") as.character(report::report(res_list$res))
        else as.character(report::report(res_list$modelo))
      }, error = function(e) "Erro ao gerar relatório.")
      writeLines(texto, file)
    }
  )

  # ---------------------------------------------------------------------------
  # ABA 6 — GLM
  # ---------------------------------------------------------------------------

  output$grafico_explorar_glm <- renderPlotly({
    req(dados(), input$var_resp_glm, input$var_grupo_glm)
    df <- dados()
    p <- ggplot(df, aes(x = as.factor(df[[input$var_grupo_glm]]),
                        y = df[[input$var_resp_glm]],
                        color = as.factor(df[[input$var_grupo_glm]]))) +
      geom_boxplot(outlier.colour = NA, alpha = 0.3) +
      geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
      scale_color_brewer(palette = "Set2") +
      labs(x = input$var_grupo_glm, y = input$var_resp_glm, color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    ggplotly_wrapper(p, height = 350)
  })

  resultado_glm <- eventReactive(input$rodar_glm, {
    # --- Suporte a AUDPC ---
    usar_audpc <- isTRUE(input$usar_audpc_glm)
    if (usar_audpc) {
      res_audpc <- audpc_calculada()
      if (is.null(res_audpc) || !res_audpc$has_rep) {
        showNotification("⚠️ Calcule a AUDPC com repetição na Aba 7 antes de usar aqui.", type = "warning")
        return(NULL)
      }
      df    <- res_audpc$df_audpc
      var_y <- "audpc"
      var_g <- res_audpc$g_col
    } else {
      req(dados(), input$var_resp_glm, input$var_grupo_glm)
      df    <- dados()
      var_y <- input$var_resp_glm
      var_g <- input$var_grupo_glm
    }
    df[[var_g]] <- as.factor(df[[var_g]])
    results <- list()

    tryCatch({
      if ("lm_bruto" %in% input$modelos_glm) {
        m <- lm(as.formula(paste(var_y, "~", var_g)), data = df)
        results$lm_bruto <- list(modelo = m, tabela = anova(m),
                                  shapiro = shapiro.test(residuals(m)))
      }
      if ("lm_sqrt" %in% input$modelos_glm) {
        m <- lm(as.formula(paste("sqrt(", var_y, ") ~", var_g)), data = df)
        results$lm_sqrt <- list(modelo = m, tabela = anova(m),
                                 shapiro = shapiro.test(residuals(m)))
      }
      if ("kruskal" %in% input$modelos_glm) {
        res <- kruskal.test(as.formula(paste(var_y, "~", var_g)), data = df)
        results$kruskal <- list(resultado = res)
      }
      if ("glm_poisson" %in% input$modelos_glm) {
        m <- glm(as.formula(paste(var_y, "~", var_g)),
                 family = poisson(link = "log"), data = df)
        em  <- emmeans(m, as.formula(paste("~", var_g)), type = "response")
        cld_res <- multcomp::cld(em, Letters = letters,
                                  adjust = input$metodo_cld_glm) |> as.data.frame()
        results$glm_poisson <- list(modelo = m, emmeans = em, cld = cld_res)
      }
      list(results = results, var_y = var_y, var_g = var_g, df = df)
    }, error = function(e) {
      showNotification(paste("Erro GLM:", e$message), type = "error"); NULL
    })
  })

  output$tabela_comparacao_glm <- renderDT({
    res_list <- resultado_glm()
    req(res_list)
    results <- res_list$results

    linhas <- list()
    if (!is.null(results$lm_bruto)) {
      an <- results$lm_bruto$tabela
      linhas[["ANOVA Bruta"]] <- data.frame(
        Abordagem = "ANOVA (dados brutos)",
        F = round(an[1, "F value"], 3),
        p.valor = formatC(an[1, "Pr(>F)"], digits = 4, format = "g"),
        Shapiro.p = formatC(results$lm_bruto$shapiro$p.value, digits = 4, format = "g")
      )
    }
    if (!is.null(results$lm_sqrt)) {
      an <- results$lm_sqrt$tabela
      linhas[["ANOVA sqrt"]] <- data.frame(
        Abordagem = "ANOVA (√count)",
        F = round(an[1, "F value"], 3),
        p.valor = formatC(an[1, "Pr(>F)"], digits = 4, format = "g"),
        Shapiro.p = formatC(results$lm_sqrt$shapiro$p.value, digits = 4, format = "g")
      )
    }
    if (!is.null(results$kruskal)) {
      linhas[["Kruskal"]] <- data.frame(
        Abordagem = "Kruskal-Wallis",
        F = round(results$kruskal$resultado$statistic, 3),
        p.valor = formatC(results$kruskal$resultado$p.value, digits = 4, format = "g"),
        Shapiro.p = "—"
      )
    }
    if (!is.null(results$glm_poisson)) {
      m <- results$glm_poisson$modelo
      an <- anova(m, test = "Chisq")
      linhas[["GLM"]] <- data.frame(
        Abordagem = "GLM Poisson",
        F = round(an[2, "Deviance"], 3),
        p.valor = formatC(an[2, "Pr(>Chi)"], digits = 4, format = "g"),
        Shapiro.p = "—"
      )
    }

    if (length(linhas) == 0) return(NULL)
    tbl <- do.call(rbind, linhas)
    rownames(tbl) <- NULL
    datatable(tbl, options = list(dom = "t", paging = FALSE),
              class = "table-striped table-sm", rownames = FALSE)
  })

  output$grafico_emmeans_glm <- renderPlotly({
    res_list <- resultado_glm()
    req(res_list, !is.null(res_list$results$glm_poisson))
    cld <- res_list$results$glm_poisson$cld
    var_g <- res_list$var_g

    p <- ggplot(cld, aes(x = reorder(.data[[var_g]], rate), y = rate, label = .group)) +
      geom_point(size = 3.5, color = "#2C7A4B") +
      geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                    width = 0.15, color = "#2C7A4B") +
      geom_text(aes(y = asymp.UCL), vjust = -0.5, size = 4,
                fontface = "bold", color = "#2C7A4B") +
      coord_flip() +
      labs(x = var_g, y = paste0("Média estimada de ", res_list$var_y, " (IC 95%)"),
           caption = "Letras iguais = não diferem (Tukey, α = 0.05)") +
      theme_minimal(base_size = 13)

    ggplotly_wrapper(p, height = 350)
  })

  output$res_lm_bruto <- renderPrint({
    res_list <- resultado_glm()
    req(res_list, !is.null(res_list$results$lm_bruto))
    cat("=== ANOVA — Dados Brutos ===\n"); print(res_list$results$lm_bruto$tabela)
    cat("\n=== Shapiro-Wilk (resíduos) ===\n"); print(res_list$results$lm_bruto$shapiro)
  })
  output$res_lm_sqrt <- renderPrint({
    res_list <- resultado_glm()
    req(res_list, !is.null(res_list$results$lm_sqrt))
    cat("=== ANOVA — Transformação √ ===\n"); print(res_list$results$lm_sqrt$tabela)
    cat("\n=== Shapiro-Wilk (resíduos) ===\n"); print(res_list$results$lm_sqrt$shapiro)
  })
  output$res_kruskal <- renderPrint({
    res_list <- resultado_glm()
    req(res_list, !is.null(res_list$results$kruskal))
    cat("=== Kruskal-Wallis ===\n"); print(res_list$results$kruskal$resultado)
  })
  output$res_glm_poisson <- renderPrint({
    res_list <- resultado_glm()
    req(res_list, !is.null(res_list$results$glm_poisson))
    cat("=== GLM Poisson — Summary ===\n"); print(summary(res_list$results$glm_poisson$modelo))
    cat("\n=== Deviância (ANOVA) ===\n"); print(anova(res_list$results$glm_poisson$modelo, test = "Chisq"))
  })

  # ---------------------------------------------------------------------------
  # ABA 7 — AUDPC
  # ---------------------------------------------------------------------------

  audpc_calculada <- eventReactive(input$calcular_audpc, {
    req(dados(), input$var_tempo_audpc, input$var_sev_audpc, input$var_grupo_audpc)
    df   <- dados()
    t_col  <- input$var_tempo_audpc
    s_col  <- input$var_sev_audpc
    g_col  <- input$var_grupo_audpc

    # Ajustar escala
    if (input$escala_sev_audpc == "pct") df[[s_col]] <- df[[s_col]] / 100

    # Calcular AUDPC manualmente (trapézios)
    calcular_audpc <- function(tempo, sev) {
      n <- length(tempo)
      if (n < 2) return(NA)
      idx  <- order(tempo)
      t    <- tempo[idx]; y <- sev[idx]
      sum(((y[-n] + y[-1]) / 2) * diff(t), na.rm = TRUE)
    }

    has_rep <- !is.null(input$var_rep_audpc) && input$var_rep_audpc != ""

    if (has_rep) {
      df_audpc <- df |>
        group_by(.data[[g_col]], .data[[input$var_rep_audpc]]) |>
        summarise(audpc = calcular_audpc(.data[[t_col]], .data[[s_col]]),
                  .groups = "drop")
    } else {
      df_audpc <- df |>
        group_by(.data[[g_col]]) |>
        summarise(audpc = calcular_audpc(.data[[t_col]], .data[[s_col]]),
                  .groups = "drop")
    }

    list(df = df, df_audpc = df_audpc, t_col = t_col, s_col = s_col,
         g_col = g_col, has_rep = has_rep)
  })

  output$grafico_curva_doenca <- renderPlotly({
    res <- audpc_calculada()
    req(res)
    df    <- res$df
    t_col <- res$t_col
    s_col <- res$s_col
    g_col <- res$g_col

    resumo <- df |>
      group_by(.data[[g_col]], .data[[t_col]]) |>
      summarise(media_sev = mean(.data[[s_col]], na.rm = TRUE) * 100,
                dp_sev = sd(.data[[s_col]], na.rm = TRUE) * 100,
                .groups = "drop")

    p <- ggplot(resumo, aes(x = .data[[t_col]], y = media_sev,
                            color = .data[[g_col]], group = .data[[g_col]])) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      geom_ribbon(aes(ymin = media_sev - dp_sev, ymax = media_sev + dp_sev,
                      fill = .data[[g_col]]), alpha = 0.15, color = NA) +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = t_col, y = "Severidade média (%)",
           color = g_col, fill = g_col,
           title = "Curva de Progresso da Doença") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())

    ggplotly_wrapper(p)
  })

  output$tabela_audpc <- renderDT({
    res <- audpc_calculada()
    req(res)
    tbl <- res$df_audpc |> mutate(across(where(is.numeric), ~round(., 3)))
    datatable(tbl, options = list(dom = "t", paging = FALSE),
              class = "table-striped table-sm", rownames = FALSE)
  })

  output$teste_audpc <- renderPrint({
    res <- audpc_calculada()
    req(res, res$has_rep)
    grupos <- unique(res$df_audpc[[res$g_col]])
    if (length(grupos) == 2) {
      cat("=== Teste t para AUDPC ===\n")
      print(t.test(audpc ~ .data[[res$g_col]], data = res$df_audpc))
    } else {
      cat("=== ANOVA para AUDPC ===\n")
      m <- lm(as.formula(paste("audpc ~", res$g_col)), data = res$df_audpc)
      print(anova(m))
    }
  })

  output$grafico_audpc_comp <- renderPlotly({
    res <- audpc_calculada()
    req(res)
    df_a  <- res$df_audpc
    g_col <- res$g_col

    resumo_a <- df_a |>
      group_by(.data[[g_col]]) |>
      summarise(media = mean(audpc, na.rm = TRUE),
                dp    = sd(audpc, na.rm = TRUE),
                n     = n(),
                ep    = dp / sqrt(n), .groups = "drop")

    p <- ggplot(resumo_a, aes(x = .data[[g_col]], y = media, fill = .data[[g_col]])) +
      geom_col(width = 0.55, alpha = 0.85, color = "white") +
      geom_errorbar(aes(ymin = media - dp, ymax = media + dp), width = 0.15) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = g_col, y = "AUDPC Média (± DP)", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none", panel.grid.minor = element_blank())

    ggplotly_wrapper(p, height = 350)
  })

  # ---------------------------------------------------------------------------
  # Sobre o App
  # ---------------------------------------------------------------------------
  observeEvent(input$about_link, {
    showModal(modalDialog(
      title = "Sobre o app Análise de Dados",
      tagList(
        p("App desenvolvido com base no conteúdo do curso FIP 606."),
        p("Cobre os principais fluxos de análise de dados em fitopatologia e agronomia:"),
        tags$ul(
          tags$li("Importação de dados (CSV, Excel, Google Sheets)"),
          tags$li("Exploração e visualização interativa"),
          tags$li("Teste t de Student e Wilcoxon"),
          tags$li("ANOVA, emmeans e Tukey"),
          tags$li("Regressão linear, polinomial e correlação"),
          tags$li("GLM com distribuição de Poisson"),
          tags$li("AUDPC — Área sob a Curva de Progresso da Doença")
        ),
        hr(),
        p(tags$b("Autoras:")),
        tags$ul(
          tags$li(tags$a("Maria Eduarda Faria Tardim", href = "https://www.linkedin.com/in/maria-eduarda-faria-tardim-86683b218/", target = "_blank")),
          tags$li(tags$a("Thalya Furtado Lopes", href = "https://www.linkedin.com/in/thalya-furtado-lopes-90a3232a9/", target = "_blank"))
        ),
        hr(),
        p(em("Construído com R + Shiny + bslib + plotly"))
      ),
      footer = modalButton("Fechar"),
      size = "m"
    ))
  })

  # ===========================================================================
  # OVERRIDE: Regressão com group_by real
  # ===========================================================================

  resultado_reg <- eventReactive(input$rodar_reg, {
    req(dados(), input$var_x_reg, input$var_y_reg)
    df  <- dados()
    x   <- input$var_x_reg
    y   <- input$var_y_reg
    grp <- if (!is.null(input$var_cor_reg) && input$var_cor_reg != "") input$var_cor_reg else NULL

    rodar_analise <- function(sub_df, grupo_nome = NULL) {
      tryCatch({
        if (input$tipo_reg %in% c("pearson", "spearman")) {
          res <- cor.test(sub_df[[x]], sub_df[[y]], method = input$tipo_reg)
          list(tipo = "cor", res = res, df = sub_df, x = x, y = y, grupo = grupo_nome)
        } else {
          formula_str <- switch(input$tipo_reg,
            linear = paste(y, "~", x),
            poly2  = paste(y, "~ poly(", x, ", 2, raw = TRUE)"),
            poly3  = paste(y, "~ poly(", x, ", 3, raw = TRUE)")
          )
          m          <- lm(as.formula(formula_str), data = sub_df)
          res_tidy   <- tidy(m)
          res_glance <- glance(m)
          list(tipo = "reg", modelo = m, tidy = res_tidy, glance = res_glance,
               df = sub_df, x = x, y = y, grupo = grupo_nome)
        }
      }, error = function(e) {
        showNotification(paste("Erro no grupo", grupo_nome, ":", e$message), type = "error")
        NULL
      })
    }

    if (!is.null(grp)) {
      df[[grp]] <- as.character(df[[grp]])
      grupos    <- sort(unique(df[[grp]]))
      resultados <- lapply(grupos, function(g) {
        sub <- df[df[[grp]] == g, ]
        rodar_analise(sub, grupo_nome = g)
      })
      names(resultados) <- grupos
      list(modo = "grouped", resultados = resultados, df = df, x = x, y = y, grp = grp)
    } else {
      res_unico <- rodar_analise(df, grupo_nome = "Global")
      list(modo = "single", resultados = list(Global = res_unico), df = df, x = x, y = y, grp = NULL)
    }
  })

  output$grafico_reg <- renderPlotly({
    res_wrap <- resultado_reg()
    req(res_wrap)
    df  <- res_wrap$df
    x   <- res_wrap$x
    y   <- res_wrap$y
    grp <- res_wrap$grp

    formula_smooth <- switch(input$tipo_reg,
      poly2 = y ~ poly(x, 2),
      poly3 = y ~ poly(x, 3),
      y ~ x
    )

    usar_grp <- !is.null(grp)

    p <- ggplot(df, aes(
      x     = .data[[x]],
      y     = .data[[y]],
      color = if (usar_grp) as.factor(.data[[grp]]) else NULL,
      fill  = if (usar_grp) as.factor(.data[[grp]]) else NULL,
      group = if (usar_grp) as.factor(.data[[grp]]) else NULL
    )) +
      geom_point(alpha = 0.7, size = 2.5) +
      scale_color_brewer(palette = "Set2") +
      scale_fill_brewer(palette  = "Set2") +
      labs(x = x, y = y,
           color = if (usar_grp) grp else NULL,
           fill  = if (usar_grp) grp else NULL) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(), legend.position = "top")

    if (input$tipo_reg %in% c("pearson", "spearman")) {
      if (usar_grp) {
        p <- p + geom_smooth(method = "lm", formula = y ~ x,
                             se = input$mostrar_ic_reg, alpha = 0.15)
      } else {
        p <- p + geom_smooth(method = "lm", formula = y ~ x,
                             se = input$mostrar_ic_reg,
                             color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
      }
    } else {
      if (usar_grp) {
        p <- p + geom_smooth(method = "lm", formula = formula_smooth,
                             se = input$mostrar_ic_reg, alpha = 0.15)
      } else {
        p <- p + geom_smooth(method = "lm", formula = formula_smooth,
                             se = input$mostrar_ic_reg,
                             color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
      }
    }

    # Anotações de R² por grupo
    if (input$mostrar_eq_reg) {
      lapply(res_wrap$resultados, function(r) {
        if (!is.null(r) && r$tipo == "reg") {
          r2  <- round(summary(r$modelo)$r.squared, 3)
          lbl <- if (!is.null(r$grupo) && r$grupo != "Global") {
            paste0(r$grupo, ": R² = ", r2)
          } else {
            paste0("R² = ", r2)
          }
          p <<- p + annotate("text",
            x = min(r$df[[x]], na.rm = TRUE),
            y = max(df[[y]], na.rm = TRUE) * (0.97 - 0.07 * which(names(res_wrap$resultados) == r$grupo)),
            label = lbl, hjust = 0, size = 3.5, fontface = "italic",
            color = RColorBrewer::brewer.pal(max(3, length(res_wrap$resultados)), "Set2")[which(names(res_wrap$resultados) == r$grupo)]
          )
        }
      })
    }

    ggplotly_wrapper(p)
  })

  output$resultado_reg_cards <- renderUI({
    res_wrap <- resultado_reg()
    req(res_wrap)

    criar_bloco_grupo <- function(r) {
      if (is.null(r)) return(NULL)
      titulo_grupo <- if (!is.null(r$grupo) && r$grupo != "Global") {
        tags$h6(style = "color:#2C7A4B; font-weight:700; margin-top:12px;",
                paste0("📌 Grupo: ", r$grupo))
      } else NULL

      cards <- if (r$tipo == "cor") {
        res <- r$res
        tagList(
          stat_card("Coeficiente r", round(res$estimate, 4), "📊"),
          stat_card("p-valor", formatC(res$p.value, digits = 4, format = "g"), "📈",
                    if (res$p.value < 0.05) "#2C7A4B" else "#e74c3c"),
          stat_card("IC 95%",
                    paste0("[", round(res$conf.int[1], 3), "; ", round(res$conf.int[2], 3), "]"), "📐")
        )
      } else {
        gl <- r$glance
        tagList(
          stat_card("R²",          round(gl$r.squared, 4),     "📊"),
          stat_card("R² ajustado", round(gl$adj.r.squared, 4), "📈"),
          stat_card("F-estatística", round(gl$statistic, 3),   "📐"),
          stat_card("p-valor (modelo)",
                    formatC(gl$p.value, digits = 4, format = "g"), "🎯",
                    if (gl$p.value < 0.05) "#2C7A4B" else "#e74c3c")
        )
      }
      tagList(titulo_grupo, cards, hr())
    }

    do.call(tagList, lapply(res_wrap$resultados, criar_bloco_grupo))
  })

  output$resultado_reg_raw <- renderPrint({
    res_wrap <- resultado_reg()
    req(res_wrap)
    for (nm in names(res_wrap$resultados)) {
      r <- res_wrap$resultados[[nm]]
      if (is.null(r)) next
      if (length(res_wrap$resultados) > 1) {
        cat("\n================================================\n")
        cat(" GRUPO:", nm, "\n")
        cat("================================================\n")
      }
      if (r$tipo == "cor") print(r$res) else print(summary(r$modelo))
    }
  })

  output$ponto_otimo <- renderUI({
    req(resultado_reg(), input$tipo_reg == "poly2")
    res_wrap <- resultado_reg()
    blocos <- lapply(names(res_wrap$resultados), function(nm) {
      r <- res_wrap$resultados[[nm]]
      if (is.null(r) || r$tipo != "reg") return(NULL)
      coefs <- coef(r$modelo)
      if (length(coefs) < 3) return(NULL)
      a <- coefs[3]; b <- coefs[2]
      x_ot <- round(-b / (2 * a), 3)
      y_ot <- round(coefs[1] + b * x_ot + a * x_ot^2, 3)
      tipo  <- if (a < 0) "Máximo" else "Mínimo"
      tagList(
        if (nm != "Global") tags$h6(style="color:#2C7A4B;font-weight:700;", paste0("📌 Grupo: ", nm)),
        stat_card(paste("Ponto de", tipo, "— X"), x_ot, "🎯"),
        stat_card(paste("Ponto de", tipo, "— Y"), y_ot, "📈"),
        hr()
      )
    })
    do.call(tagList, blocos)
  })

  # Histogramas de inspeção das variáveis na aba Regressão
  output$histogramas_reg <- renderPlotly({
    res_wrap <- resultado_reg()
    req(res_wrap)
    df <- res_wrap$df
    x  <- res_wrap$x
    y  <- res_wrap$y

    get_title <- function(var) {
      if (var == "inc") {
        "Histograma de incidência"
      } else if (var == "scl") {
        "Histograma de scl"
      } else {
        paste("Histograma de", var)
      }
    }

    p1 <- ggplot(df, aes(x = .data[[x]])) +
      geom_histogram(bins = 10, fill = "lightgray", color = "black") +
      labs(title = get_title(x), x = x, y = "Frequência") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_blank() # Vamos usar anotações do plotly para os títulos dos subplots
      )

    p2 <- ggplot(df, aes(x = .data[[y]])) +
      geom_histogram(bins = 10, fill = "lightgray", color = "black") +
      labs(title = get_title(y), x = y, y = "Frequência") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        plot.title = element_blank() # Vamos usar anotações do plotly para os títulos dos subplots
      )

    p1_ly <- ggplotly(p1)
    p2_ly <- ggplotly(p2)

    subplot(p1_ly, p2_ly, nrows = 1, titleX = TRUE, titleY = TRUE, margin = 0.07) |>
      layout(
        annotations = list(
          list(
            x = 0.23, y = 1.05, text = get_title(x), showarrow = FALSE,
            xref = "paper", yref = "paper", font = list(size = 14, family = "Inter, sans-serif")
          ),
          list(
            x = 0.77, y = 1.05, text = get_title(y), showarrow = FALSE,
            xref = "paper", yref = "paper", font = list(size = 14, family = "Inter, sans-serif")
          )
        ),
        margin = list(l = 50, r = 20, t = 60, b = 60),
        font   = list(family = "Inter, sans-serif", size = 12),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             displaylogo = FALSE)
  })

  # ---------------------------------------------------------------------------
  # ABA 8 — NÃO PARAMÉTRICOS
  # ---------------------------------------------------------------------------
  resultado_np <- eventReactive(input$rodar_np, {
    # --- Suporte a AUDPC ---
    usar_audpc <- isTRUE(input$usar_audpc_np)
    if (usar_audpc) {
      res_audpc <- audpc_calculada()
      if (is.null(res_audpc) || !res_audpc$has_rep) {
        showNotification("⚠️ Calcule a AUDPC com repetição na Aba 7 antes de usar aqui.", type = "warning")
        return(NULL)
      }
      df    <- res_audpc$df_audpc
      var_y <- "audpc"
      var_g <- res_audpc$g_col
    } else {
      req(dados(), input$var_resp_np, input$var_grupo_np)
      df    <- dados()
      var_y <- input$var_resp_np
      var_g <- input$var_grupo_np
    }
    df[[var_g]] <- as.factor(df[[var_g]])
    tipo <- input$tipo_teste_np
    res  <- list(df = df, var_y = var_y, var_g = var_g, tipo = tipo)

    tryCatch({
      if (tipo == "mann") {
        res$test <- wilcox.test(as.formula(paste(var_y, "~", var_g)), data = df)
      } else if (tipo == "wilcox_par") {
        grupos <- unique(df[[var_g]])
        g1 <- df[[var_y]][df[[var_g]] == grupos[1]]
        g2 <- df[[var_y]][df[[var_g]] == grupos[2]]
        res$test <- wilcox.test(g1, g2, paired = TRUE)
      } else if (tipo == "kruskal") {
        res$test <- kruskal.test(as.formula(paste(var_y, "~", var_g)), data = df)
        if (requireNamespace("FSA", quietly = TRUE)) {
          res$posthoc <- FSA::dunnTest(as.formula(paste(var_y, "~", var_g)), data = df, method = "holm")
          res$posthoc_type <- "dunn"
        } else {
          res$posthoc <- tryCatch({
            pairwise.wilcox.test(df[[var_y]], df[[var_g]], p.adjust.method = "holm")
          }, error = function(e) {
            paste("Erro ao rodar pós-hoc Wilcoxon:", e$message)
          })
          res$posthoc_type <- "wilcox_pairwise"
        }
      } else if (tipo == "friedman") {
        req(input$var_bloco_np)
        var_b <- input$var_bloco_np
        res$test <- friedman.test(as.formula(paste(var_y, "~", var_g, "|", var_b)), data = df)
      }
      res
    }, error = function(e) {
      showNotification(paste("Erro:", e$message), type = "error")
      NULL
    })
  })

  output$plot_np <- renderPlotly({
    res <- resultado_np()
    req(res)
    p <- ggplot(res$df, aes(x = .data[[res$var_g]], y = .data[[res$var_y]], color = .data[[res$var_g]])) +
      geom_boxplot(outlier.colour = NA, alpha = 0.3) +
      geom_jitter(width = 0.15, size = 2, alpha = 0.6) +
      scale_color_brewer(palette = "Set2") +
      labs(x = res$var_g, y = res$var_y) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    ggplotly_wrapper(p, height = 400)
  })

  output$resumo_np <- renderPrint({
    res <- resultado_np()
    req(res)
    print(res$test)
  })

  output$posthoc_np <- renderPrint({
    res <- resultado_np()
    req(res, res$tipo == "kruskal")
    if (!is.null(res$posthoc)) {
      if (is.character(res$posthoc)) {
        cat(res$posthoc)
      } else if (!is.null(res$posthoc_type) && res$posthoc_type == "wilcox_pairwise") {
        cat("Aviso: O pacote 'FSA' não está instalado no R. Apresentando fallback com Teste de Wilcoxon Pareado:\n\n")
        print(res$posthoc)
        cat("\nPara obter o Teste Post-Hoc de Dunn, execute: install.packages('FSA')\n")
      } else {
        print(res$posthoc)
      }
    }
  })

  output$posthoc_friedman <- renderPrint({
    res <- resultado_np()
    req(res, res$tipo == "friedman")
    cat("Post-hoc de Friedman não implementado nativamente no R base sem pacotes adicionais específicos (como PMCMRplus).\n")
    cat("O teste global indica se há diferença entre os grupos.\n")
  })

  output$report_np <- renderPrint({
    res <- resultado_np()
    req(res)
    cat("Gerando relatório descritivo...\n\n")
    tryCatch({
      print(report::report(res$test))
    }, error = function(e) cat("Erro ao gerar relatório:", e$message))
  })

  output$download_report_np <- downloadHandler(
    filename = function() { paste0("relatorio_np_", Sys.Date(), ".txt") },
    content = function(file) {
      res <- resultado_np()
      req(res)
      texto <- tryCatch(as.character(report::report(res$test)), error = function(e) "Erro ao gerar relatório.")
      writeLines(texto, file)
    }
  )

  # ---------------------------------------------------------------------------
  # ABA 9 — EDITOR GRÁFICO
  # ---------------------------------------------------------------------------
  grafico_custom_gerado <- reactiveVal(NULL)

  observeEvent(input$atualizar_grafico, {

    # ===========================================================
    # ANOVA (Aba 4)
    # ===========================================================
    if (input$fonte_grafico == "anova") {
      res <- resultado_anova()
      if (is.null(res)) {
        showNotification("Rode a ANOVA primeiro na Aba 4.", type = "warning")
        return(NULL)
      }
      
      cld <- res$cld
      df <- res$df
      var_y <- res$var_y
      
      # Calcular SD original
      if (input$delineamento %in% c("dic", "dbc")) {
        var_t <- res$var_t
        sd_data <- df |> group_by(.data[[var_t]]) |> summarise(SD = sd(.data[[var_y]], na.rm = TRUE))
        cld <- left_join(cld, sd_data, by = var_t)
        cld$x_var <- cld[[var_t]]
        cld$group_var <- cld[[var_t]]
      } else if (input$delineamento %in% c("fatorial", "splitplot", "misto")) {
        f_a <- res$f_principal
        f_b <- res$f_desdobramento
        sd_data <- df |> group_by(.data[[f_a]], .data[[f_b]]) |> summarise(SD = sd(.data[[var_y]], na.rm = TRUE))
        cld <- left_join(cld, sd_data, by = c(f_a, f_b))
        cld$x_var <- cld[[f_a]]
        cld$group_var <- cld[[f_b]]
      }

      ci_lower <- if ("lower.CL" %in% colnames(cld)) "lower.CL" else (if ("asymp.LCL" %in% colnames(cld)) "asymp.LCL" else grep("LCL|lower", colnames(cld), value = TRUE)[1])
      ci_upper <- if ("upper.CL" %in% colnames(cld)) "upper.CL" else (if ("asymp.UCL" %in% colnames(cld)) "asymp.UCL" else grep("UCL|upper", colnames(cld), value = TRUE)[1])
      if (!is.null(ci_lower) && !is.na(ci_lower) && ci_lower %in% colnames(cld) && ci_lower != "lower.CL") cld$lower.CL <- cld[[ci_lower]]
      if (!is.null(ci_upper) && !is.na(ci_upper) && ci_upper %in% colnames(cld) && ci_upper != "upper.CL") cld$upper.CL <- cld[[ci_upper]]

      cld$ymin <- switch(input$graf_erro,
                         "sd" = cld$emmean - cld$SD,
                         "se" = cld$emmean - cld$SE,
                         "ci" = cld$lower.CL)
      cld$ymax <- switch(input$graf_erro,
                         "sd" = cld$emmean + cld$SD,
                         "se" = cld$emmean + cld$SE,
                         "ci" = cld$upper.CL)

      cld$.group <- trimws(cld$.group)

      # Construir o gráfico baseado no tipo selecionado
      if (input$graf_tipo == "coluna") {
        if (input$delineamento %in% c("dic", "dbc")) {
          p <- ggplot(cld, aes(x = as.factor(x_var), y = emmean)) +
            geom_col(width = 0.6, fill = "#2C7A4B", color = "black") +
            geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2)
        } else {
          p <- ggplot(cld, aes(x = as.factor(x_var), y = emmean, fill = as.factor(group_var))) +
            geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black") +
            geom_errorbar(aes(ymin = ymin, ymax = ymax), position = position_dodge(width = 0.8), width = 0.2) +
            scale_fill_brewer(palette = "Set2") +
            labs(fill = "Legenda")
        }
      } else if (input$graf_tipo == "boxplot") {
        if (input$delineamento %in% c("dic", "dbc")) {
          var_t <- res$var_t
          p <- ggplot(df, aes(x = as.factor(.data[[var_t]]), y = .data[[var_y]])) +
            geom_boxplot(width = 0.6, fill = "#2C7A4B", color = "black", alpha = 0.4, outlier.colour = NA) +
            geom_point(position = position_jitter(width = 0.1), alpha = 0.5, color = "#2C7A4B", size = 2) +
            geom_point(data = cld, aes(x = as.factor(x_var), y = emmean), shape = 18, size = 4, color = "black") +
            geom_errorbar(data = cld, aes(x = as.factor(x_var), y = emmean, ymin = ymin, ymax = ymax), width = 0.2)
        } else {
          f_a = res$f_principal
          f_b = res$f_desdobramento
          p <- ggplot(df, aes(x = as.factor(.data[[f_a]]), y = .data[[var_y]], fill = as.factor(.data[[f_b]]))) +
            geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.4, color = "black", outlier.colour = NA) +
            geom_point(aes(color = as.factor(.data[[f_b]])), position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), alpha = 0.5, size = 2) +
            geom_point(data = cld, aes(x = as.factor(x_var), y = emmean, group = as.factor(group_var)), 
                       position = position_dodge(width = 0.8), shape = 18, size = 4, color = "black") +
            geom_errorbar(data = cld, aes(x = as.factor(x_var), y = emmean, ymin = ymin, ymax = ymax, group = as.factor(group_var)), 
                          position = position_dodge(width = 0.8), width = 0.2) +
            scale_fill_brewer(palette = "Set2") +
            scale_color_brewer(palette = "Set2") +
            labs(fill = "Legenda", color = "Legenda")
        }
      } else if (input$graf_tipo == "ponto") {
        if (input$delineamento %in% c("dic", "dbc")) {
          p <- ggplot(cld, aes(x = as.factor(x_var), y = emmean, group = 1)) +
            geom_line(color = "#2C7A4B", linewidth = 1) +
            geom_point(size = 4, color = "#2C7A4B") +
            geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "#2C7A4B")
        } else {
          p <- ggplot(cld, aes(x = as.factor(x_var), y = emmean, color = as.factor(group_var), group = as.factor(group_var))) +
            geom_line(position = position_dodge(width = 0.3), linewidth = 1) +
            geom_point(position = position_dodge(width = 0.3), size = 4) +
            geom_errorbar(aes(ymin = ymin, ymax = ymax), position = position_dodge(width = 0.3), width = 0.2) +
            scale_color_brewer(palette = "Set2") +
            labs(color = "Legenda")
        }
      }

      # Adicionar as letras de médias e valores numéricos acima das barras de erro
      nudge_y_base <- max(abs(cld$ymax), na.rm = TRUE) * 0.04
      dodge_w <- if (input$graf_tipo == "ponto") 0.3 else 0.8

      if (input$delineamento %in% c("dic", "dbc")) {
        if (input$graf_valores) {
          p <- p + geom_text(data = cld, aes(x = as.factor(x_var), y = ymax + nudge_y_base, label = sprintf("%.2f", emmean)), 
                             size = 4.2, fontface = "italic", color = "black")
        }
        if (input$graf_letras) {
          letra_y <- if (input$graf_valores) (cld$ymax + nudge_y_base * 2.5) else (cld$ymax + nudge_y_base)
          p <- p + geom_text(data = cld, aes(x = as.factor(x_var), y = letra_y, label = .group), 
                             size = 5, fontface = "bold", color = "black")
        }
      } else {
        if (input$graf_valores) {
          p <- p + geom_text(data = cld, aes(x = as.factor(x_var), y = ymax + nudge_y_base, group = as.factor(group_var), label = sprintf("%.2f", emmean)), 
                             position = position_dodge(width = dodge_w), size = 3.8, fontface = "italic", color = "black")
        }
        if (input$graf_letras) {
          letra_y <- if (input$graf_valores) (cld$ymax + nudge_y_base * 2.5) else (cld$ymax + nudge_y_base)
          p <- p + geom_text(data = cld, aes(x = as.factor(x_var), y = letra_y, group = as.factor(group_var), label = .group), 
                             position = position_dodge(width = dodge_w), size = 4, fontface = "bold", color = "black")
        }
      }

      p <- p + labs(x = input$graf_xlab, y = input$graf_ylab, title = input$graf_title) +
        theme_minimal(base_size = input$graf_font) +
        theme(axis.text.x = element_text(angle = as.numeric(input$graf_angle), hjust = ifelse(input$graf_angle == "0", 0.5, 1)),
              axis.line = element_line(color = "black"),
              panel.grid.major.x = element_blank())

      grafico_custom_gerado(p)

    # ===========================================================
    # TESTE T / WILCOXON (Aba 3)
    # ===========================================================
    } else if (input$fonte_grafico == "teste_t") {
      res <- resultado_tt()
      if (is.null(res)) {
        showNotification("Rode o Teste t primeiro na Aba 3.", type = "warning")
        return(NULL)
      }
      df    <- res$df
      var_y <- res$var_y
      var_g <- res$var_g

      # Calcular resumo por grupo
      resumo <- df |>
        group_by(.data[[var_g]]) |>
        summarise(
          media = mean(.data[[var_y]], na.rm = TRUE),
          SD    = sd(.data[[var_y]], na.rm = TRUE),
          SE    = SD / sqrt(n()),
          n     = n(),
          ymin_sd = media - SD, ymax_sd = media + SD,
          ymin_se = media - SE, ymax_se = media + SE,
          .groups = "drop"
        )
      resumo$ymin <- switch(input$graf_erro, "sd" = resumo$ymin_sd, "se" = resumo$ymin_se, "ci" = resumo$ymin_se)
      resumo$ymax <- switch(input$graf_erro, "sd" = resumo$ymax_sd, "se" = resumo$ymax_se, "ci" = resumo$ymax_se)

      if (input$graf_tipo %in% c("coluna", "ponto")) {
        p <- ggplot(resumo, aes(x = as.factor(.data[[var_g]]), y = media, fill = as.factor(.data[[var_g]]))) +
          geom_col(width = 0.6, color = "black", alpha = 0.85) +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
          scale_fill_brewer(palette = "Set2") +
          labs(fill = NULL)
      } else {
        p <- ggplot(df, aes(x = as.factor(.data[[var_g]]), y = .data[[var_y]], fill = as.factor(.data[[var_g]]))) +
          geom_boxplot(alpha = 0.55, outlier.colour = NA, width = 0.6, color = "black") +
          geom_jitter(width = 0.12, alpha = 0.6, size = 2.2, aes(color = as.factor(.data[[var_g]]))) +
          scale_fill_brewer(palette = "Set2") +
          scale_color_brewer(palette = "Set2") +
          labs(fill = NULL, color = NULL)
      }

      if (input$graf_valores) {
        nudge_tt <- max(resumo$ymax, na.rm = TRUE) * 0.04
        p <- p + geom_text(data = resumo,
                           aes(x = as.factor(.data[[var_g]]), y = ymax + nudge_tt,
                               label = sprintf("%.2f", media)),
                           size = 4, fontface = "italic", color = "black", inherit.aes = FALSE)
      }

      # Anotar p-valor do teste
      pval_lab <- tryCatch({
        pv <- res$resultado$p.value
        if (!is.na(pv)) paste0("p = ", formatC(pv, digits = 3, format = "g")) else ""
      }, error = function(e) "")
      if (nchar(pval_lab) > 0) {
        p <- p + annotate("text", x = 1.5, y = max(resumo$ymax, na.rm = TRUE) * 1.08,
                          label = pval_lab, size = 4.5, fontface = "bold.italic", color = "#2C7A4B")
      }

      p <- p +
        labs(x = input$graf_xlab, y = input$graf_ylab, title = input$graf_title) +
        theme_minimal(base_size = input$graf_font) +
        theme(axis.text.x = element_text(angle = as.numeric(input$graf_angle),
                                          hjust = ifelse(input$graf_angle == "0", 0.5, 1)),
              axis.line = element_line(color = "black"),
              legend.position = "none",
              panel.grid.major.x = element_blank())

      grafico_custom_gerado(p)

    # ===========================================================
    # REGRESSÃO / CORRELAÇÃO (Aba 5)
    # ===========================================================
    } else if (input$fonte_grafico == "regressao") {
      res <- resultado_reg()
      if (is.null(res)) {
        showNotification("Rode a Regressão/Correlação primeiro na Aba 5.", type = "warning")
        return(NULL)
      }
      df <- res$df
      x  <- res$x
      y  <- res$y

      p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
        geom_point(alpha = 0.7, size = 2.8, color = "#2C7A4B") +
        theme_minimal(base_size = input$graf_font) +
        theme(panel.grid.minor = element_blank(),
              axis.line = element_line(color = "black"))

      if (res$tipo == "cor") {
        p <- p + geom_smooth(method = "lm", se = TRUE, color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
        r_val  <- round(res$res$estimate, 3)
        pv_val <- formatC(res$res$p.value, digits = 3, format = "g")
        eq_lab <- paste0("r = ", r_val, "\np = ", pv_val)
      } else {
        formula_smooth <- switch(res$tipo,
          poly2 = y ~ poly(x, 2), poly3 = y ~ poly(x, 3), y ~ x
        )
        p <- p + geom_smooth(method = "lm", formula = formula_smooth,
                             se = TRUE, color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15)
        r2_val <- round(res$glance$r.squared, 3)
        pv_val <- formatC(res$glance$p.value, digits = 3, format = "g")
        eq_lab <- paste0("R\u00b2 = ", r2_val, "\np = ", pv_val)
      }

      if (input$graf_valores) {
        p <- p + annotate("text",
                          x = min(df[[x]], na.rm = TRUE),
                          y = max(df[[y]], na.rm = TRUE),
                          label = eq_lab, hjust = 0, vjust = 1,
                          size = input$graf_font / 3.2, color = "#2C7A4B", fontface = "italic")
      }

      p <- p + labs(x = input$graf_xlab, y = input$graf_ylab, title = input$graf_title)

      grafico_custom_gerado(p)

    # ===========================================================
    # GLM POISSON (Aba 6)
    # ===========================================================
    } else if (input$fonte_grafico == "glm") {
      res_list <- resultado_glm()
      if (is.null(res_list) || is.null(res_list$results$glm_poisson)) {
        showNotification("Rode o GLM Poisson primeiro na Aba 6 (marque 'GLM Poisson' nas abordagens).", type = "warning")
        return(NULL)
      }
      cld   <- res_list$results$glm_poisson$cld
      var_g <- res_list$var_g
      var_y <- res_list$var_y

      # Detectar colunas de IC (rate + asymp.LCL/asymp.UCL ou emmean + lower.CL/upper.CL)
      y_col  <- if ("rate" %in% colnames(cld)) "rate" else "emmean"
      lci_col <- if ("asymp.LCL" %in% colnames(cld)) "asymp.LCL" else "lower.CL"
      uci_col <- if ("asymp.UCL" %in% colnames(cld)) "asymp.UCL" else "upper.CL"

      cld$y_val  <- cld[[y_col]]
      cld$y_low  <- switch(input$graf_erro,
                           "sd" = cld$y_val - cld$SE,
                           "se" = cld$y_val - cld$SE,
                           "ci" = cld[[lci_col]])
      cld$y_high <- switch(input$graf_erro,
                           "sd" = cld$y_val + cld$SE,
                           "se" = cld$y_val + cld$SE,
                           "ci" = cld[[uci_col]])
      cld$.group <- trimws(cld$.group)

      if (input$graf_tipo == "coluna") {
        p <- ggplot(cld, aes(x = reorder(.data[[var_g]], y_val), y = y_val, fill = as.factor(.data[[var_g]]))) +
          geom_col(width = 0.65, color = "black", alpha = 0.85) +
          geom_errorbar(aes(ymin = y_low, ymax = y_high), width = 0.2) +
          scale_fill_brewer(palette = "Set2") +
          labs(fill = NULL)
      } else if (input$graf_tipo == "boxplot") {
        df_raw <- res_list$df
        p <- ggplot(df_raw, aes(x = as.factor(.data[[var_g]]), y = .data[[var_y]], fill = as.factor(.data[[var_g]]))) +
          geom_boxplot(alpha = 0.55, outlier.colour = NA, width = 0.6, color = "black") +
          geom_jitter(width = 0.12, alpha = 0.6, size = 2.2, aes(color = as.factor(.data[[var_g]]))) +
          scale_fill_brewer(palette = "Set2") +
          scale_color_brewer(palette = "Set2") +
          labs(fill = NULL, color = NULL)
      } else {
        p <- ggplot(cld, aes(x = reorder(.data[[var_g]], y_val), y = y_val, color = as.factor(.data[[var_g]]), group = 1)) +
          geom_line(linewidth = 1) +
          geom_point(size = 4) +
          geom_errorbar(aes(ymin = y_low, ymax = y_high), width = 0.2) +
          scale_color_brewer(palette = "Set2") +
          labs(color = NULL)
      }

      nudge_glm <- max(cld$y_high, na.rm = TRUE) * 0.04
      if (input$graf_valores) {
        p <- p + geom_text(data = cld,
                           aes(x = reorder(.data[[var_g]], y_val), y = y_high + nudge_glm,
                               label = sprintf("%.2f", y_val)),
                           size = 4, fontface = "italic", color = "black", inherit.aes = FALSE)
      }
      if (input$graf_letras) {
        letra_y_glm <- if (input$graf_valores) (cld$y_high + nudge_glm * 2.5) else (cld$y_high + nudge_glm)
        p <- p + geom_text(data = cld,
                           aes(x = reorder(.data[[var_g]], y_val), y = letra_y_glm, label = .group),
                           size = 5, fontface = "bold", color = "black", inherit.aes = FALSE)
      }

      p <- p +
        labs(x = input$graf_xlab, y = input$graf_ylab, title = input$graf_title) +
        theme_minimal(base_size = input$graf_font) +
        theme(axis.text.x = element_text(angle = as.numeric(input$graf_angle),
                                          hjust = ifelse(input$graf_angle == "0", 0.5, 1)),
              axis.line = element_line(color = "black"),
              legend.position = "none",
              panel.grid.major.x = element_blank())

      grafico_custom_gerado(p)

    # ===========================================================
    # NÃO PARAMÉTRICOS (Aba 8)
    # ===========================================================
    } else if (input$fonte_grafico == "nao_param") {
      res <- resultado_np()
      if (is.null(res)) {
        showNotification("Rode o teste Não Paramétrico primeiro na Aba 8.", type = "warning")
        return(NULL)
      }
      df    <- res$df
      var_y <- res$var_y
      var_g <- res$var_g

      # Resumo por grupo (mediana e IQR)
      resumo_np <- df |>
        group_by(.data[[var_g]]) |>
        summarise(
          mediana = median(.data[[var_y]], na.rm = TRUE),
          Q1 = quantile(.data[[var_y]], 0.25, na.rm = TRUE),
          Q3 = quantile(.data[[var_y]], 0.75, na.rm = TRUE),
          n  = n(),
          .groups = "drop"
        )
      resumo_np$ymin <- resumo_np$Q1
      resumo_np$ymax <- resumo_np$Q3

      if (input$graf_tipo == "coluna") {
        p <- ggplot(resumo_np, aes(x = as.factor(.data[[var_g]]), y = mediana, fill = as.factor(.data[[var_g]]))) +
          geom_col(width = 0.6, color = "black", alpha = 0.85) +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
          scale_fill_brewer(palette = "Set2") +
          labs(fill = NULL)
      } else if (input$graf_tipo == "ponto") {
        p <- ggplot(resumo_np, aes(x = as.factor(.data[[var_g]]), y = mediana, color = as.factor(.data[[var_g]]), group = 1)) +
          geom_line(linewidth = 1) +
          geom_point(size = 4) +
          geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
          scale_color_brewer(palette = "Set2") +
          labs(color = NULL)
      } else {
        p <- ggplot(df, aes(x = as.factor(.data[[var_g]]), y = .data[[var_y]], fill = as.factor(.data[[var_g]]))) +
          geom_boxplot(alpha = 0.55, outlier.colour = NA, width = 0.6, color = "black") +
          geom_jitter(width = 0.12, alpha = 0.6, size = 2.2, aes(color = as.factor(.data[[var_g]]))) +
          scale_fill_brewer(palette = "Set2") +
          scale_color_brewer(palette = "Set2") +
          labs(fill = NULL, color = NULL)
      }

      # Anotar estatística e p-valor
      pval_np_lab <- tryCatch({
        pv <- res$test$p.value
        if (!is.na(pv)) paste0("p = ", formatC(pv, digits = 3, format = "g")) else ""
      }, error = function(e) "")
      if (nchar(pval_np_lab) > 0) {
        n_grupos <- length(unique(df[[var_g]]))
        p <- p + annotate("text",
                          x = ceiling(n_grupos / 2),
                          y = max(df[[var_y]], na.rm = TRUE) * 1.05,
                          label = pval_np_lab, size = 4.5,
                          fontface = "bold.italic", color = "#2C7A4B")
      }

      if (input$graf_valores) {
        nudge_np <- max(resumo_np$ymax, na.rm = TRUE) * 0.05
        p <- p + geom_text(data = resumo_np,
                           aes(x = as.factor(.data[[var_g]]), y = ymax + nudge_np,
                               label = sprintf("%.2f", mediana)),
                           size = 4, fontface = "italic", color = "black", inherit.aes = FALSE)
      }

      p <- p +
        labs(x = input$graf_xlab, y = input$graf_ylab, title = input$graf_title) +
        theme_minimal(base_size = input$graf_font) +
        theme(axis.text.x = element_text(angle = as.numeric(input$graf_angle),
                                          hjust = ifelse(input$graf_angle == "0", 0.5, 1)),
              axis.line = element_line(color = "black"),
              legend.position = "none",
              panel.grid.major.x = element_blank())

      grafico_custom_gerado(p)
    }
  })

  output$plot_custom <- renderPlot({
    req(grafico_custom_gerado())
    grafico_custom_gerado()
  })

  output$download_grafico <- downloadHandler(
    filename = function() { paste0("grafico_publicavel_", Sys.Date(), ".png") },
    content = function(file) {
      req(grafico_custom_gerado())
      ggsave(file, plot = grafico_custom_gerado(), width = 10, height = 6, dpi = 300)
    }
  )


  # ---------------------------------------------------------------------------
  # ABA 10 — RELATÓRIO HTML
  # ---------------------------------------------------------------------------

  # Função interna: converte um ggplot para string base64 PNG embutida em <img>
  gg_para_img_html <- function(p, width = 800, height = 450, res = 120) {
    tmp <- tempfile(fileext = ".png")
    tryCatch({
      ggplot2::ggsave(tmp, plot = p, width = width / res, height = height / res,
                      dpi = res, bg = "white")
      b64 <- base64enc::base64encode(tmp)
      paste0('<img src="data:image/png;base64,', b64,
             '" style="max-width:100%;border-radius:6px;margin:10px 0;" />')
    }, error = function(e) {
      paste0('<p style="color:red;">Gráfico indisponível: ', e$message, '</p>')
    }, finally = {
      if (file.exists(tmp)) unlink(tmp)
    })
  }

  # Bloco HTML estilizado de seção
  secao_html <- function(titulo, conteudo_html, cor = "#2C7A4B") {
    paste0(
      '<div style="margin-bottom:2rem;border-left:5px solid ', cor,
      ';padding-left:1.2rem;">',
      '<h2 style="color:', cor, ';font-family:Outfit,sans-serif;font-size:1.3rem;">',
      titulo, '</h2>', conteudo_html, '</div>'
    )
  }

  # Converte data.frame para tabela HTML simples
  df_para_html <- function(df, digits = 4) {
    if (is.null(df) || nrow(df) == 0) return('<p><em>Sem dados disponíveis.</em></p>')
    df <- as.data.frame(df)
    # arredonda numérico
    df <- df |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                             ~round(., digits)))
    rows <- apply(df, 1, function(r) {
      paste0('<tr>', paste0('<td style="padding:4px 10px;border:1px solid #dee2e6;">',
                            r, '</td>', collapse = ''), '</tr>')
    })
    header <- paste0('<tr style="background:#2C7A4B;color:white;">',
                     paste0('<th style="padding:6px 10px;border:1px solid #dee2e6;">',
                            names(df), '</th>', collapse = ''), '</tr>')
    paste0(
      '<div style="overflow-x:auto;"><table style="border-collapse:collapse;',
      'width:100%;font-size:0.88rem;font-family:Inter,sans-serif;">',
      header, paste(rows, collapse = ''), '</table></div>'
    )
  }

  relatorio_html_conteudo <- reactiveVal(NULL)

  observeEvent(input$gerar_relatorio, {
    relatorio_html_conteudo(NULL)  # reset

    partes <- list()

    # ---- Cabeçalho ----
    autor_html <- ""
    if (!is.null(input$rel_autor) && trimws(input$rel_autor) != "") {
      autor_html <- paste0('<strong>', htmltools::htmlEscape(trimws(input$rel_autor)), '</strong> &bull; ')
    }
    
    partes[[length(partes) + 1]] <- paste0(
      '<div style="text-align:center;padding:2rem 0 1rem;">',
      '<h1 style="color:#2C7A4B;font-family:Outfit,sans-serif;font-size:2rem;">',
      htmltools::htmlEscape(input$rel_titulo), '</h1>',
      '<p style="font-size:1rem;color:#636e72;">',
      autor_html,
      htmltools::htmlEscape(input$rel_instituicao), ' &bull; ',
      htmltools::htmlEscape(input$rel_data),
      '</p><hr style="border-color:#2C7A4B;"/></div>'
    )

    # ---- Seção: Dados ----
    if (isTRUE(input$rel_inc_dados) && !is.null(dados())) {
      df <- dados()
      info <- paste0(
        '<ul style="font-size:0.95rem;">',
        '<li><strong>Observações:</strong> ', nrow(df), '</li>',
        '<li><strong>Variáveis:</strong> ', ncol(df), '</li>',
        '<li><strong>Colunas:</strong> ', paste(names(df), collapse = ', '), '</li>',
        '</ul>'
      )
      # Primeiras 15 linhas
      prev <- df_para_html(head(df, 15))
      partes[[length(partes) + 1]] <- secao_html(
        "📋 Resumo dos Dados",
        paste0(info, '<p><em>Primeiras linhas:</em></p>', prev)
      )
    }

    # ---- Seção: Exploração ----
    if (isTRUE(input$rel_inc_exp) && !is.null(dados()) &&
        !is.null(input$var_resp_exp) && input$var_resp_exp != "") {
      tryCatch({
        df <- dados_transf()
        var_y <- if (input$transf_exp == "none") input$var_resp_exp else
          paste0(input$var_resp_exp, "_transf")
        usar_grupo <- !is.null(input$var_grupo_exp) && input$var_grupo_exp != ""

        p <- ggplot(df, aes(
          x = if (usar_grupo) .data[[input$var_grupo_exp]] else factor("Todos"),
          y = .data[[var_y]],
          fill = if (usar_grupo) .data[[input$var_grupo_exp]] else NULL
        )) +
          stat_summary(fun = mean, geom = "col", alpha = 0.8, color = "white") +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2,
                       linewidth = 0.8, color = "black") +
          geom_jitter(width = 0.12, alpha = 0.6, size = 2.2) +
          scale_fill_brewer(palette = "Set2") +
          labs(x = if (usar_grupo) input$var_grupo_exp else "",
               y = var_y, fill = NULL) +
          theme_minimal(base_size = 12) +
          theme(legend.position = "none", panel.grid.minor = element_blank())

        img_html <- gg_para_img_html(p)
        partes[[length(partes) + 1]] <- secao_html("📊 Exploração / Descritiva", img_html)
      }, error = function(e) NULL)
    }

    # ---- Seção: Teste t ----
    if (isTRUE(input$rel_inc_tt)) {
      tryCatch({
        res <- resultado_tt()
        if (!is.null(res)) {
          r   <- res$resultado
          pv  <- r$p.value
          sig <- if (!is.na(pv) && pv < 0.05) "✅ Significativo" else "❌ Não significativo"
          info <- paste0(
            '<ul style="font-size:0.93rem;">',
            '<li><strong>Teste:</strong> ', r$method, '</li>',
            '<li><strong>Estatística:</strong> ',
              names(r$statistic), ' = ', round(r$statistic, 4), '</li>',
            if (!is.null(r$parameter))
              paste0('<li><strong>GL:</strong> ', round(r$parameter, 2), '</li>')
            else '',
            '<li><strong>p-valor:</strong> ', formatC(pv, digits=4, format="g"), '</li>',
            '<li><strong>Conclusão:</strong> ', sig, '</li>',
            '</ul>'
          )
          # Gráfico boxplot dos grupos
          df <- res$df; vy <- res$var_y; vg <- res$var_g
          p <- ggplot(df, aes(x = .data[[vg]], y = .data[[vy]],
                              fill = .data[[vg]])) +
            geom_boxplot(alpha = 0.55, outlier.colour = NA, color = "black") +
            geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
            scale_fill_brewer(palette = "Set2") +
            labs(x = vg, y = vy, fill = NULL) +
            theme_minimal(base_size = 12) +
            theme(legend.position = "none")
          img_html <- gg_para_img_html(p)
          partes[[length(partes) + 1]] <- secao_html(
            "🧪 Teste t / Wilcoxon",
            paste0(info, img_html)
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: ANOVA ----
    if (isTRUE(input$rel_inc_anova)) {
      tryCatch({
        res <- resultado_anova()
        if (!is.null(res)) {
          tbl_av <- res$tabela |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
          tbl_em <- as.data.frame(res$cld) |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 3)))

          cld <- res$cld
          cld$.group <- trimws(cld$.group)
          ci_lower <- if ("lower.CL" %in% colnames(cld)) "lower.CL" else
            if ("asymp.LCL" %in% colnames(cld)) "asymp.LCL" else NULL
          ci_upper <- if ("upper.CL" %in% colnames(cld)) "upper.CL" else
            if ("asymp.UCL" %in% colnames(cld)) "asymp.UCL" else NULL
          if (!is.null(ci_lower) && ci_lower %in% colnames(cld) && ci_lower != "lower.CL")
            cld$lower.CL <- cld[[ci_lower]]
          if (!is.null(ci_upper) && ci_upper %in% colnames(cld) && ci_upper != "upper.CL")
            cld$upper.CL <- cld[[ci_upper]]

          p <- if ("lower.CL" %in% colnames(cld)) {
            ggplot(cld, aes(x = reorder(.data[[res$var_t]], emmean), y = emmean)) +
              geom_point(size = 3.5, color = "#2C7A4B") +
              geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                            width = 0.15, color = "#2C7A4B", linewidth = 0.8) +
              geom_text(aes(label = .group, y = upper.CL),
                        vjust = -0.8, fontface = "bold", size = 4, color = "#2C7A4B") +
              coord_flip() +
              labs(x = res$var_t, y = paste0("Média ajustada — ", res$var_y)) +
              theme_minimal(base_size = 12) +
              theme(panel.grid.minor = element_blank())
          } else NULL

          img_html <- if (!is.null(p)) gg_para_img_html(p) else ""
          partes[[length(partes) + 1]] <- secao_html(
            "📐 ANOVA",
            paste0(
              '<h3 style="font-size:1rem;color:#555;">Tabela ANOVA</h3>',
              df_para_html(tbl_av),
              '<h3 style="font-size:1rem;color:#555;margin-top:1rem;">Médias Ajustadas (emmeans)</h3>',
              df_para_html(tbl_em),
              img_html
            )
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: Regressão ----
    if (isTRUE(input$rel_inc_reg)) {
      tryCatch({
        res_list <- resultado_reg()
        if (!is.null(res_list)) {
          df <- res_list$df; x <- res_list$x; y <- res_list$y
          formula_smooth <- switch(input$tipo_reg,
            poly2 = y ~ poly(x, 2), poly3 = y ~ poly(x, 3), y ~ x)
          p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
            geom_point(alpha = 0.7, size = 2.5, color = "#2C7A4B") +
            geom_smooth(method = "lm", formula = formula_smooth,
                        se = TRUE, color = "#2C7A4B", fill = "#2C7A4B", alpha = 0.15) +
            labs(x = x, y = y) +
            theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank())

          if (res_list$tipo == "cor") {
            r <- res_list$res
            info <- paste0(
              '<ul style="font-size:0.93rem;">',
              '<li><strong>Método:</strong> ', r$method, '</li>',
              '<li><strong>r:</strong> ', round(r$estimate, 4), '</li>',
              '<li><strong>p-valor:</strong> ', formatC(r$p.value, digits=4, format="g"), '</li>',
              '</ul>'
            )
          } else {
            gl <- res_list$glance
            info <- paste0(
              '<ul style="font-size:0.93rem;">',
              '<li><strong>R²:</strong> ', round(gl$r.squared, 4), '</li>',
              '<li><strong>R² ajustado:</strong> ', round(gl$adj.r.squared, 4), '</li>',
              '<li><strong>F:</strong> ', round(gl$statistic, 3), '</li>',
              '<li><strong>p-valor:</strong> ', formatC(gl$p.value, digits=4, format="g"), '</li>',
              '</ul>'
            )
          }
          img_html <- gg_para_img_html(p)
          partes[[length(partes) + 1]] <- secao_html(
            "📈 Regressão / Correlação",
            paste0(info, img_html)
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: GLM ----
    if (isTRUE(input$rel_inc_glm)) {
      tryCatch({
        res_list <- resultado_glm()
        if (!is.null(res_list) && !is.null(res_list$results)) {
          info_parts <- list()
          if (!is.null(res_list$results$lm_bruto)) {
            an <- as.data.frame(anova(res_list$results$lm_bruto$modelo)) |>
              dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
            info_parts[[length(info_parts)+1]] <-
              paste0('<h3 style="font-size:1rem;color:#555;">ANOVA Bruta</h3>',
                     df_para_html(an))
          }
          if (!is.null(res_list$results$glm_poisson)) {
            cld_g <- res_list$results$glm_poisson$cld
            info_parts[[length(info_parts)+1]] <-
              paste0('<h3 style="font-size:1rem;color:#555;">GLM Poisson — Médias Estimadas</h3>',
                     df_para_html(as.data.frame(cld_g)))
          }
          partes[[length(partes) + 1]] <- secao_html(
            "🦠 GLM", paste(info_parts, collapse = "")
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: Não Paramétricos ----
    if (isTRUE(input$rel_inc_np)) {
      tryCatch({
        res <- resultado_np()
        if (!is.null(res)) {
          r <- res$test
          info <- paste0(
            '<ul style="font-size:0.93rem;">',
            '<li><strong>Teste:</strong> ', r$method, '</li>',
            '<li><strong>Estatística:</strong> ',
              names(r$statistic), ' = ', round(r$statistic, 4), '</li>',
            '<li><strong>p-valor:</strong> ', formatC(r$p.value, digits=4, format="g"), '</li>',
            '</ul>'
          )
          df <- res$df; vy <- res$var_y; vg <- res$var_g
          p <- ggplot(df, aes(x = as.factor(.data[[vg]]), y = .data[[vy]],
                              fill = as.factor(.data[[vg]]))) +
            geom_boxplot(alpha = 0.55, outlier.colour = NA, color = "black") +
            geom_jitter(width = 0.12, alpha = 0.6, size = 2) +
            scale_fill_brewer(palette = "Set2") +
            labs(x = vg, y = vy, fill = NULL) +
            theme_minimal(base_size = 12) +
            theme(legend.position = "none")
          img_html <- gg_para_img_html(p)
          partes[[length(partes) + 1]] <- secao_html(
            "⚖️ Testes Não Paramétricos",
            paste0(info, img_html)
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: AUDPC ----
    if (isTRUE(input$rel_inc_audpc)) {
      tryCatch({
        res <- audpc_calculada()
        if (!is.null(res) && !is.null(res$df_curva)) {
          df_c <- res$df_curva
          g_col <- res$g_col
          t_col <- res$t_col
          s_col <- res$s_col
          p <- ggplot(df_c, aes(x = .data[[t_col]], y = .data[[s_col]],
                                color = .data[[g_col]], group = .data[[g_col]])) +
            stat_summary(fun = mean, geom = "line", linewidth = 1.1) +
            stat_summary(fun = mean, geom = "point", size = 3) +
            scale_color_brewer(palette = "Set2") +
            labs(x = t_col, y = s_col, color = g_col,
                 title = "Curva de Progresso da Doença (média por grupo)") +
            theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank())
          img_html <- gg_para_img_html(p)

          tbl_html <- if (!is.null(res$df_audpc)) df_para_html(res$df_audpc) else ""
          partes[[length(partes) + 1]] <- secao_html(
            "🌱 AUDPC — Progresso da Doença",
            paste0(img_html,
                   '<h3 style="font-size:1rem;color:#555;margin-top:1rem;">Tabela AUDPC</h3>',
                   tbl_html)
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Seção: Gráfico Customizado ----
    if (isTRUE(input$rel_inc_graficos)) {
      tryCatch({
        p <- grafico_custom_gerado()
        if (!is.null(p)) {
          img_html <- gg_para_img_html(p, width = 900, height = 500)
          partes[[length(partes) + 1]] <- secao_html(
            "🎨 Gráfico Customizado (Editor Gráfico)", img_html
          )
        }
      }, error = function(e) NULL)
    }

    # ---- Rodapé ----
    partes[[length(partes) + 1]] <- paste0(
      '<hr style="border-color:#dfe6e9;"/>',
      '<p style="text-align:center;font-size:0.8rem;color:#b2bec3;">',
      'Relatório gerado pelo App Análise Estatística Interativa &bull; ',
      format(Sys.time(), "%d/%m/%Y %H:%M"), '</p>'
    )

    # Montar HTML completo
    html_body <- paste(partes, collapse = "\n")
    html_full <- paste0(
      '<!DOCTYPE html><html lang="pt-BR"><head>',
      '<meta charset="UTF-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1"/>',
      '<title>', htmltools::htmlEscape(input$rel_titulo), '</title>',
      '<link rel="preconnect" href="https://fonts.googleapis.com">',
      '<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;600&',
      'family=Outfit:wght@700&display=swap" rel="stylesheet">',
      '<style>',
      'body{font-family:Inter,sans-serif;max-width:960px;margin:0 auto;',
      'padding:2rem;color:#2d3436;background:#f8faf9;}',
      'h1,h2,h3{font-family:Outfit,sans-serif;}',
      'table{width:100%;border-collapse:collapse;margin:1rem 0;}',
      'th{background:#2C7A4B;color:white;padding:6px 10px;}',
      'td{padding:5px 10px;border:1px solid #dee2e6;}',
      'tr:nth-child(even){background:#f1f8f4;}',
      'img{max-width:100%;height:auto;box-shadow:0 2px 8px rgba(0,0,0,.12);}',
      '@media print{body{max-width:100%;padding:1rem;}}',
      '</style></head><body>',
      html_body,
      '</body></html>'
    )

    relatorio_html_conteudo(html_full)
    showNotification("✅ Relatório gerado com sucesso!", type = "message")
  })

  output$status_relatorio <- renderUI({
    if (is.null(relatorio_html_conteudo())) {
      div(class = "alert alert-warning",
          icon("exclamation-triangle"),
          " Configure as opções e clique em ",
          tags$b("▶ Gerar Relatório"), " para visualizar o relatório.")
    } else {
      div(class = "alert alert-success",
          icon("check-circle"),
          " Relatório gerado! Você pode baixá-lo ou imprimi-lo como PDF com ",
          tags$b("Ctrl+P"), " no browser.")
    }
  })

  output$preview_relatorio_iframe <- renderUI({
    req(relatorio_html_conteudo())
    tags$iframe(srcdoc = relatorio_html_conteudo(),
                style = "width:100%; height:600px; border:none;")
  })

  output$download_html <- downloadHandler(
    filename = function() {
      paste0("relatorio_fip606_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      html <- relatorio_html_conteudo()
      if (is.null(html)) html <- "<p>Nenhum relatório gerado ainda. Clique em 'Gerar Relatório' primeiro.</p>"
      writeLines(html, file, useBytes = TRUE)
    },
    contentType = "text/html"
  )

  # ---------------------------------------------------------------------------
  # ABA 11 — EXPORTAR DADOS
  # ---------------------------------------------------------------------------

  tabela_exportar <- reactive({
    fonte <- input$export_fonte
    tryCatch({
      switch(fonte,
        brutos = {
          req(dados())
          dados()
        },
        resumo = {
          req(dados(), input$var_resp_exp)
          df <- dados()
          var_y <- input$var_resp_exp
          usar_grupo <- !is.null(input$var_grupo_exp) && input$var_grupo_exp != ""
          if (usar_grupo) {
            df |>
              dplyr::group_by(.data[[input$var_grupo_exp]]) |>
              dplyr::summarise(
                n       = dplyr::n(),
                Media   = round(mean(.data[[var_y]], na.rm = TRUE), 4),
                Mediana = round(median(.data[[var_y]], na.rm = TRUE), 4),
                DP      = round(sd(.data[[var_y]], na.rm = TRUE), 4),
                EP      = round(DP / sqrt(n), 4),
                Min     = round(min(.data[[var_y]], na.rm = TRUE), 4),
                Max     = round(max(.data[[var_y]], na.rm = TRUE), 4),
                .groups = "drop"
              )
          } else {
            df |>
              dplyr::summarise(
                n       = dplyr::n(),
                Media   = round(mean(.data[[var_y]], na.rm = TRUE), 4),
                Mediana = round(median(.data[[var_y]], na.rm = TRUE), 4),
                DP      = round(sd(.data[[var_y]], na.rm = TRUE), 4),
                EP      = round(DP / sqrt(n), 4),
                Min     = round(min(.data[[var_y]], na.rm = TRUE), 4),
                Max     = round(max(.data[[var_y]], na.rm = TRUE), 4)
              )
          }
        },
        anova = {
          res <- resultado_anova()
          req(res)
          as.data.frame(res$tabela) |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
        },
        emmeans = {
          res <- resultado_anova()
          req(res)
          as.data.frame(res$cld) |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
        },
        audpc = {
          res <- audpc_calculada()
          req(res, res$df_audpc)
          as.data.frame(res$df_audpc) |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
        },
        teste_t = {
          res <- resultado_tt()
          req(res)
          data.frame(
            Teste = res$resultado$method,
            Estatistica = as.numeric(res$resultado$statistic),
            GL = if(!is.null(res$resultado$parameter)) as.numeric(res$resultado$parameter) else NA,
            p_valor = res$resultado$p.value
          ) |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 6)))
        },
        regressao = {
          res <- resultado_reg()
          req(res)
          if (res$tipo == "cor") {
            data.frame(
              Teste = res$res$method,
              Estimativa_r = as.numeric(res$res$estimate),
              p_valor = res$res$p.value
            ) |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 6)))
          } else {
            as.data.frame(res$tidy) |>
              dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
          }
        },
        glm = {
          res <- resultado_glm()
          req(res, res$results$glm_poisson)
          as.data.frame(res$results$glm_poisson$cld) |>
            dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 4)))
        },
        nao_param = {
          res <- resultado_np()
          req(res)
          data.frame(
            Teste      = res$test$method,
            Estatistica = as.numeric(res$test$statistic),
            p_valor    = res$test$p.value
          ) |> dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(., 6)))
        }
      )
    }, error = function(e) {
      showNotification(
        paste0("Rode a análise correspondente antes de exportar: ", e$message),
        type = "warning"
      )
      NULL
    })
  })

  output$preview_exportar <- renderDT({
    df <- tabela_exportar()
    req(df)
    datatable(
      df,
      options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip"),
      class = "table-striped table-hover table-sm",
      rownames = FALSE
    )
  })

  output$download_exportar <- downloadHandler(
    filename = function() {
      ext <- switch(input$export_formato,
                    csv  = ".csv",
                    xlsx = ".xlsx",
                    txt  = ".txt")
      paste0("fip606_", input$export_fonte, "_", Sys.Date(), ext)
    },
    content = function(file) {
      df <- tabela_exportar()
      req(df)
      fmt <- input$export_formato
      if (fmt == "csv") {
        sep_val <- input$export_sep
        dec_val <- input$export_dec
        write.table(df, file, sep = sep_val, dec = dec_val,
                    row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
      } else if (fmt == "xlsx") {
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "Dados")
          openxlsx::writeData(wb, "Dados", df)
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        } else {
          # Fallback: CSV com extensão .xlsx (avisando)
          write.csv(df, file, row.names = FALSE)
          showNotification("Pacote 'openxlsx' não encontrado. Arquivo salvo como CSV.",
                           type = "warning")
        }
      } else {
        # TXT formatado
        sink_output <- capture.output({
          cat("===  Exportação de Dados ==========================\n")
          cat("Tabela:", input$export_fonte, "\n")
          cat("Data:", as.character(Sys.Date()), "\n")
          cat("============================================================\n\n")
          print(df)
        })
        writeLines(sink_output, file)
      }
    }
  )

}


# =============================================================================
# RODAR
# =============================================================================

shinyApp(ui = ui, server = server)
