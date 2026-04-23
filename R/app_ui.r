## ============================================================================
## app_ui.r ÃÂÃÂÃÂÃÂ¢ÃÂÃÂÃÂÃÂ?microecoshiny Application UI
## ============================================================================

#' microecoshiny Application UI
#'
#' @import shiny
#' @import bs4Dash
#' @importFrom shinyAce aceEditor
#' @return The Shiny UI definition
app_ui <- function(request = NULL) {
  # Set max upload size to 500MB
  options(shiny.maxRequestSize = 500 * 1024^2)

  tagList(
    golem_add_external_resources(),

    bs4Dash::dashboardPage(
      skin = "light",
      freshTheme = fresh::create_theme(
        fresh::bs4dash_vars(
          primary = "#2C3E50",
          danger = "#E74C3C",
          success = "#27AE60",
          info = "#3498DB",
          warning = "#F39C12"
        )
      ),

      header = bs4Dash::dashboardHeader(
        title = "\U0001f9ec microecoshiny",
        tags$script(HTML("
          window.addEventListener(\"DOMContentLoaded\", function() {
            setTimeout(function() {
              var navbar = document.querySelector(\".navbar-nav.ml-auto\") ||
                           document.querySelector(\".navbar-nav.ms-auto\") ||
                           document.querySelector(\".navbar-right\");
              if (!navbar) {
                var header = document.querySelector(\".main-header\");
                if (header) {
                  navbar = document.createElement(\"ul\");
                  navbar.className = \"navbar-nav ml-auto\";
                  header.appendChild(navbar);
                }
              }
              if (navbar) {
                var langBtn = document.createElement(\"li\");
                langBtn.className = \"nav-item\";
                langBtn.innerHTML = \"<div id=\\\"lang_switcher\\\" style=\\\"display:flex;gap:2px;margin-right:15px;margin-top:10px;\\\">\" +
                  \"<button type=\\\"button\\\" class=\\\"btn btn-sm\\\" id=\\\"lang_en_btn\\\" style=\\\"border-radius:4px;padding:4px 12px;font-size:13px;cursor:pointer;\\\">EN</button>\" +
                  \"<button type=\\\"button\\\" class=\\\"btn btn-sm\\\" id=\\\"lang_zh_btn\\\" style=\\\"border-radius:4px;padding:4px 12px;font-size:13px;cursor:pointer;\\\">\u4e2d\u6587</button>\" +
                  \"</div>\";
                navbar.appendChild(langBtn);

                document.getElementById(\"lang_en_btn\").addEventListener(\"click\", function() {
                  if (window.Shiny) {
                    Shiny.setInputValue(\"language_switch\", \"en\");
                  }
                });
                document.getElementById(\"lang_zh_btn\").addEventListener(\"click\", function() {
                  if (window.Shiny) {
                    Shiny.setInputValue(\"language_switch\", \"zh\");
                  }
                });

                function _updateButtonStyle(lang) {
                  var enBtn = document.getElementById(\"lang_en_btn\");
                  var zhBtn = document.getElementById(\"lang_zh_btn\");
                  var isDark = document.body.classList.contains(\"dark-mode\");
                  var textColor = isDark ? \"#fff\" : \"#333\";
                  var borderColor = isDark ? \"rgba(255,255,255,0.4)\" : \"rgba(0,0,0,0.2)\";
                  var activeBg = isDark ? \"rgba(255,255,255,0.25)\" : \"rgba(0,0,0,0.1)\";

                  [enBtn, zhBtn].forEach(function(btn) {
                    btn.style.color = textColor;
                    btn.style.border = \"1px solid \" + borderColor;
                    btn.style.background = \"transparent\";
                    btn.style.fontWeight = \"normal\";
                  });

                  if (lang === \"en\") {
                    enBtn.style.background = activeBg;
                    enBtn.style.fontWeight = \"bold\";
                  } else {
                    zhBtn.style.background = activeBg;
                    zhBtn.style.fontWeight = \"bold\";
                  }
                }

                var themeObserver = new MutationObserver(function(mutations) {
                  var savedLang = localStorage.getItem(\"microecoshiny_lang\") || \"zh\";
                  _updateButtonStyle(savedLang);
                });
                themeObserver.observe(document.body, { attributes: true, attributeFilter: [\"class\"] });

                var savedLang = localStorage.getItem(\"microecoshiny_lang\") || \"zh\";
                _updateButtonStyle(savedLang);
                if (window.Shiny) {
                  Shiny.setInputValue(\"language_switch_init\", savedLang);
                }
              }
            }, 500);
          });
        "))
      ),

      sidebar = bs4Dash::dashboardSidebar(
        disable = FALSE,
        collapsed = FALSE,
        minWidth = 260,
        maxWidth = 400,
        bs4Dash::sidebarMenu(
          id = "sidebar_menu",
          bs4Dash::menuItem(
            text = "\U0001f4c1 数据管理",
            tabName = "data_mgmt",
            icon = icon("folder"),
            startExpanded = TRUE,
            bs4Dash::menuSubItem(text = "数据导入", tabName = "mod_import", icon = icon("upload")),
            bs4Dash::menuSubItem(text = "数据预处理", tabName = "mod_preprocess", icon = icon("broom")),
            bs4Dash::menuSubItem(text = "数据标准化", tabName = "mod_norm", icon = icon("balance-scale")),
            bs4Dash::menuSubItem(text = "数据导出", tabName = "mod_export", icon = icon("download"))
          ),
          bs4Dash::menuItem(
            text = "\U0001f4ca 基础分析",
            tabName = "basic_analysis",
            icon = icon("chart-bar"),
            startExpanded = FALSE,
            bs4Dash::menuSubItem(text = "类群丰度", tabName = "mod_abund", icon = icon("table")),
            bs4Dash::menuSubItem(text = "群落组成", tabName = "mod_composition", icon = icon("chart-pie")),
            bs4Dash::menuSubItem(text = "\u03b1 多样性", tabName = "mod_alpha", icon = icon("leaf")),
            bs4Dash::menuSubItem(text = "\u03b2 多样性", tabName = "mod_beta", icon = icon("project-diagram")),
            bs4Dash::menuSubItem(text = "核心微生物组", tabName = "mod_core", icon = icon("bullseye"))
          ),
          bs4Dash::menuItem(
            text = "\U0001f52c 高级分析",
            tabName = "advanced_analysis",
            icon = icon("flask"),
            startExpanded = FALSE,
            bs4Dash::menuSubItem(text = "差异丰度", tabName = "mod_diff", icon = icon("balance-scale-left")),
            bs4Dash::menuSubItem(text = "环境因子关联", tabName = "mod_env", icon = icon("link")),
            bs4Dash::menuSubItem(text = "网络分析", tabName = "mod_network", icon = icon("share-alt")),
            bs4Dash::menuSubItem(text = "零模型", tabName = "mod_nullmodel", icon = icon("random")),
            bs4Dash::menuSubItem(text = "功能预测", tabName = "mod_func", icon = icon("dna")),
            bs4Dash::menuSubItem(text = "机器学习", tabName = "mod_ml", icon = icon("robot"))
          ),
          bs4Dash::menuItem(
            text = "\U0001f9ec 多组学",
            tabName = "multiomics",
            icon = icon("atom"),
            bs4Dash::menuSubItem(text = "宏基因组与代谢组", tabName = "mod_multiomics", icon = icon("vials"))
          ),
          bs4Dash::menuItem(
            text = "\U0001f4dd 生成代码",
            tabName = "mod_codes",
            icon = icon("code")
          )
        ),
        tags$div(
          class = "sidebar-footer",
          style = "position: absolute; bottom: 0; left: 0; right: 0; padding: 10px 15px; border-top: 1px solid #dee2e6; font-size: 0.8rem;",
          uiOutput("sidebar_data_status")
        )
      ),

      body = bs4Dash::dashboardBody(
        # Language switch handler for sidebar
        tags$script(HTML("
          var sidebarTranslations = {
            '\U0001f4c1 数据管理': '\U0001f4c1 Data Management',
            '数据导入': 'Import',
            '数据预处理': 'Preprocess',
            '数据标准化': 'Normalization',
            '数据导出': 'Export',
            '\U0001f4ca 基础分析': '\U0001f4ca Basic Analysis',
            '类群丰度': 'Abundance',
            '群落组成': 'Composition',
            '\u03b1 多样性': '\u03b1 Diversity',
            '\u03b2 多样性': '\u03b2 Diversity',
            '核心微生物组': 'Core Microbiome',
            '\U0001f52c 高级分析': '\U0001f52c Advanced Analysis',
            '差异丰度': 'Differential Abundance',
            '环境因子关联': 'Env Correlation',
            '网络分析': 'Network Analysis',
            '零模型': 'Null Model',
            '功能预测': 'Functional Prediction',
            '机器学习': 'Machine Learning',
            '\U0001f9ec 多组学': '\U0001f9ec Multi-omics',
            '宏基因组与代谢组': 'Multi-omics Analysis',
            '\U0001f4dd 生成代码': '\U0001f4dd Generate Code'
          };

          // Build reverse mapping (en -> zh)
          var sidebarTranslationsReverse = {};
          for (var zh in sidebarTranslations) {
            sidebarTranslationsReverse[sidebarTranslations[zh]] = zh;
          }

          Shiny.addCustomMessageHandler('updateSidebarLang', function(message) {
            var lang = message.lang;
            var isZh = lang === 'zh';
            var dict = isZh ? sidebarTranslationsReverse : sidebarTranslations;

            document.querySelectorAll('.sidebar-menu a, .nav-treeview a, .sidebar-menu li.treeview > a').forEach(function(el) {
              var currentText = el.textContent.trim();
              var newText = dict[currentText];
              if (newText) {
                var iconEl = el.querySelector('i');
                var iconHtml = iconEl ? iconEl.outerHTML : '';
                el.innerHTML = iconHtml + ' ' + newText;
              }
            });
          });
        ")),

        # IDE-style layout wrapper
        tags$div(
          class = "ide-layout-wrapper",

          bs4Dash::tabItems(
            bs4Dash::tabItem(tabName = "mod_import", uiOutput("mod_import_content")),
            bs4Dash::tabItem(tabName = "mod_preprocess", uiOutput("mod_preprocess_content")),
            bs4Dash::tabItem(tabName = "mod_norm", uiOutput("mod_norm_content")),
            bs4Dash::tabItem(tabName = "mod_abund", uiOutput("mod_abund_content")),
            bs4Dash::tabItem(tabName = "mod_composition", uiOutput("mod_composition_content")),
            bs4Dash::tabItem(tabName = "mod_alpha", uiOutput("mod_alpha_content")),
            bs4Dash::tabItem(tabName = "mod_beta", uiOutput("mod_beta_content")),
            bs4Dash::tabItem(tabName = "mod_core", uiOutput("mod_core_content")),
            bs4Dash::tabItem(tabName = "mod_diff", uiOutput("mod_diff_content")),
            bs4Dash::tabItem(tabName = "mod_env", uiOutput("mod_env_content")),
            bs4Dash::tabItem(tabName = "mod_network", uiOutput("mod_network_content")),
            bs4Dash::tabItem(tabName = "mod_nullmodel", uiOutput("mod_nullmodel_content")),
            bs4Dash::tabItem(tabName = "mod_func", uiOutput("mod_func_content")),
            bs4Dash::tabItem(tabName = "mod_ml", uiOutput("mod_ml_content")),
            bs4Dash::tabItem(tabName = "mod_multiomics", uiOutput("mod_multiomics_content")),
            bs4Dash::tabItem(tabName = "mod_export", uiOutput("mod_export_content")),
            bs4Dash::tabItem(tabName = "mod_workspace", uiOutput("mod_workspace_content")),
            bs4Dash::tabItem(tabName = "mod_codes", uiOutput("mod_codes_content"))
          ),

          # IDE-style right panel
          tags$div(
            id = "sidebar-resize-handle",
            title = "\u62d6\u62fd\u8c03\u6574\u5bbd\u5ea6/\u70b9\u51fb\u5173\u95ed",
            onclick = "window.toggleSidebar()"
          ),
          tags$div(
            id = "obj-browser-sidebar",
            uiOutput("obj_browser_ui")
          ),
          tags$div(
            id = "sidebar_toggle_btn",
            onclick = "window.toggleSidebar()",
            icon("chevron-right")
          )
        )
      ),

      controlbar = bs4Dash::dashboardControlbar(
        collapsed = TRUE,
        overlay = TRUE,
        id = "controlbar",
        tags$div(
          style = "padding: 20px;",
          h4("\u2699 \u8bbe\u7f6e Settings"),
          hr(),
          shinyWidgets::materialSwitch(inputId = "auto_scroll_codes", label = "\u81ea\u52a8\u6eda\u52a8\u4ee3\u7801", value = TRUE, status = "primary"),
          br(),
          shinyWidgets::materialSwitch(inputId = "show_code_in_modules", label = "\u5728\u5404\u6a21\u5757\u663e\u793a\u4ee3\u7801", value = FALSE, status = "info"),
          br(),
          shinyWidgets::materialSwitch(inputId = "interactive_plots", label = "\u4ea4\u4e92\u5f0f\u56fe\u8868 (plotly)", value = FALSE, status = "success"),
          hr(),
          h5("\u4e3b\u9898 Theme"),
          shinyWidgets::pickerInput(inputId = "theme_picker", label = "\u9009\u62e9\u4e3b\u9898", choices = c("\u6d45\u8272 Light" = "light", "\u6df1\u8272 Dark" = "dark"), selected = "light"),
        )
      ),
      title = "microecoshiny"
    ) # end dashboardPage
  )
}

#' Add external Resources
#' @import shiny
#' @import golem
golem_add_external_resources <- function() {
  www_path <- system.file("app/www", package = "microecoshiny")
  if (www_path == "") www_path <- "inst/app/www"
    addResourcePath("www", www_path)
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$style(HTML(
      ':root{--sidebar-width:208px;}'
    )),
    tags$script(HTML(paste0(
      '(function(){',
      'function initResize(){',
      'var handle=document.getElementById("sidebar-resize-handle");',
      'if(!handle||handle.dataset.bound)return;',
      'handle.dataset.bound="1";',
      'var startX,startW;',
      'handle.addEventListener("mousedown",function(e){',
      'e.preventDefault();',
      'startX=e.clientX;',
      'var sw=getComputedStyle(document.documentElement).getPropertyValue("--sidebar-width");',
      'startW=parseInt(sw)||208;',
      'handle.classList.add("resizing");',
      'document.body.style.cursor="col-resize";',
      'document.body.style.userSelect="none";',
      'function onMove(ev){',
      'var dx=startX-ev.clientX;',
      'var newW=Math.max(200,Math.min(500,startW+dx));',
      'document.documentElement.style.setProperty("--sidebar-width",newW+"px");',
      'var btn=document.getElementById("sidebar_toggle_btn");',
      'if(btn)btn.style.right=newW+"px";',
      '}',
      'function onUp(){',
      'handle.classList.remove("resizing");',
      'document.body.style.cursor="";',
      'document.body.style.userSelect="";',
      'document.removeEventListener("mousemove",onMove);',
      'document.removeEventListener("mouseup",onUp);',
      'var cw=getComputedStyle(document.documentElement).getPropertyValue("--sidebar-width");',
      'localStorage.setItem("microecoshiny_sidebar_width",parseInt(cw)||208);',
      '}',
      'document.addEventListener("mousemove",onMove);',
      'document.addEventListener("mouseup",onUp);',
      '});',
      '}',
      'if(document.readyState==="loading"){',
      'document.addEventListener("DOMContentLoaded",initResize);',
      '}else{initResize();}',
      'window.toggleSidebar=function(){',
      'var wrapper=document.querySelector(".ide-layout-wrapper");',
      'var s=document.getElementById("obj-browser-sidebar");',
      'var b=document.getElementById("sidebar_toggle_btn");',
      'var h=document.getElementById("sidebar-resize-handle");',
      'if(s)s.classList.toggle("sidebar-collapsed");',
      'if(b){',
      'b.classList.toggle("toggle-collapsed");',
      'if(b.classList.contains("toggle-collapsed")){',
      'b.style.right="0px";',
      '}else{',
      'var sw=parseInt(getComputedStyle(document.documentElement).getPropertyValue("--sidebar-width"))||208;',
      'b.style.right=sw+"px";',
      '}',
      '}',
      'if(h){',
      'if(s&&s.classList.contains("sidebar-collapsed")){',
      'h.style.right="0px";',
      'h.style.display="none";',
      '}else{',
      'h.style.display="block";',
      'var sw=parseInt(getComputedStyle(document.documentElement).getPropertyValue("--sidebar-width"))||208;',
      'h.style.right=sw+"px";',
      '}',
      '}',
      'if(wrapper)wrapper.classList.toggle("sidebar-collapsed");',
      'var c=s&&s.classList.contains("sidebar-collapsed");',
      'if(c){document.body.classList.add("sidebar-collapsed");}',
      'else{document.body.classList.remove("sidebar-collapsed");}',
      'localStorage.setItem("microecoshiny_sidebar",c?"collapsed":"open");',
      '};',
      'if(localStorage.getItem("microecoshiny_sidebar")==="collapsed"){',
      'setTimeout(function(){',
      'var wrapper=document.querySelector(".ide-layout-wrapper");',
      'var s=document.getElementById("obj-browser-sidebar");',
      'var b=document.getElementById("sidebar_toggle_btn");',
      'var h=document.getElementById("sidebar-resize-handle");',
      'if(s)s.classList.remove("sidebar-collapsed");',
      'if(b)b.classList.remove("toggle-collapsed");',
      'if(wrapper)wrapper.classList.remove("sidebar-collapsed");',
      'document.body.classList.remove("sidebar-collapsed");',
      'localStorage.setItem("microecoshiny_sidebar","open");',
      'if(b){var sw=parseInt(getComputedStyle(document.documentElement).getPropertyValue("--sidebar-width"))||208;b.style.right=sw+"px";}',
      '},100);',
      '}',
      'window.toggleTheme=function(){',
      'var body=document.body;',
      'var sidebar=document.querySelector(".main-sidebar");',
      'var navbar=document.querySelector(".navbar");',
      'var isDark=body.classList.toggle("dark-mode");',
      'if(sidebar){sidebar.classList.toggle("sidebar-dark-primary");sidebar.classList.toggle("sidebar-light-primary");}',
      'if(navbar){navbar.classList.toggle("navbar-dark");navbar.classList.toggle("navbar-light");}',
      'localStorage.setItem("microecoshiny_theme",isDark?"dark":"light");',
      '};',
      'window.addEventListener("DOMContentLoaded",function(){',
      'if(localStorage.getItem("microecoshiny_theme")==="dark")window.toggleTheme();',
      '});',
      '})();',
      'window.toggleTheme=function(){',
      'var body=document.body;',
      'var sidebar=document.querySelector(".main-sidebar");',
      'var navbar=document.querySelector(".navbar");',
      'var isDark=body.classList.toggle("dark-mode");',
      'if(sidebar){sidebar.classList.toggle("sidebar-dark-primary");sidebar.classList.toggle("sidebar-light-primary");}',
      'if(navbar){navbar.classList.toggle("navbar-dark");navbar.classList.toggle("navbar-light");}',
      'localStorage.setItem("microecoshiny_theme",isDark?"dark":"light");',
      '};',
      'window.addEventListener("DOMContentLoaded",function(){',
      'if(localStorage.getItem("microecoshiny_theme")==="dark")window.toggleTheme();',
      '});',
      'window.pinCodes=function(){',
      'var p=document.getElementById("pinned_codes_panel");',
      'var b=document.getElementById("pin_codes_btn");',
      'if(p){p.style.display="flex";b.style.display="none";}',
      '};',
      'window.unpinCodes=function(){',
      'var p=document.getElementById("pinned_codes_panel");',
      'var b=document.getElementById("pin_codes_btn");',
      'if(p){p.style.display="none";b.style.display="block";}',
      '};',
      'window.addEventListener("DOMContentLoaded",function(){',
      'setInterval(function(){',
      'var a=document.querySelector(".sidebar-menu .active a");',
      'var b=document.getElementById("pin_codes_btn");',
      'if(b&&a&&a.getAttribute("data-value")==="mod_codes")b.style.display="none";',
      'else if(b)b.style.display="block";',
      '},500);',
      '});',
      'window.objBrowserToggleAll=function(collapse){',
      'var s=document.getElementById("obj-browser-sidebar");',
      'if(!s)return;',
      's.querySelectorAll(".card-header [data-toggle=collapse]").forEach(function(btn){',
      'var href=btn.getAttribute("href")||btn.getAttribute("data-target");',
      'if(!href)return;',
      'var t=document.querySelector(href);',
      'if(!t)return;',
      'if(collapse&&t.classList.contains("show"))btn.click();',
      'if(!collapse&&!t.classList.contains("show"))btn.click();',
      '});',
      '};',
      'Shiny.addCustomMessageHandler("objBrowserToggleAll",function(msg){',
      'if(typeof objBrowserToggleAll==="function")objBrowserToggleAll(msg.collapse);',
      '});'
    ))),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}

