# 加载必要的包
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(readxl)
library(rio)
library(stringr)

# 全局配置变量
config <- list(
  # 登录凭证（默认值）
  login_name = "",
  login_pwd = "",
  # 医院ID（会在登录后获取）
  hospital_id = NULL
)

# 环境配置
env_configs <- list(
  test = list(
    base_url = "https://test.ylbz.tj.gov.cn:7777/csb/1.0.0"
  ),
  prod = list(
    base_url = "https://tps.ylbz.tj.gov.cn/csb/1.0.0"
  )
)

# 日志函数
log_message <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] [%s] %s\n", timestamp, toupper(level), message))
}

# 统一API调用函数
call_api <- function(endpoint, params, current_env) {
  url <- paste0(current_env$base_url, "/", endpoint)

  # 构建请求数据
  request_body <- list(
    loginname = config$login_name,
    loginpwd = config$login_pwd,
    hospitalid = config$hospital_id
  )

  # 添加额外参数
  request_body <- c(request_body, params)

  # 移除NULL值
  request_body <- request_body[!sapply(request_body, is.null)]

  # 转换为JSON
  json_body <- toJSON(request_body, auto_unbox = TRUE, na = "null")

  # 设置请求头
  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  log_message("info", sprintf("调用接口: %s", endpoint))

  tryCatch(
    {
      # 发送POST请求
      response <- POST(
        url = url,
        body = json_body,
        add_headers(headers),
        encode = "json",
        timeout(30) # 30秒超时
      )

      # 检查HTTP状态
      if (status_code(response) != 200) {
        stop(sprintf("HTTP错误: %s", status_code(response)))
      }

      # 解析响应内容
      response_content <- content(response, "text", encoding = "UTF-8")
      response_json <- fromJSON(response_content)

      # 检查API返回状态
      if (response_json$returnCode != "0001") {
        log_message(
          "error",
          sprintf(
            "API错误: %s - %s",
            response_json$returnCode,
            response_json$returnMessage
          )
        )
        return(NULL)
      }

      log_message(
        "info",
        sprintf("接口调用成功: %s", response_json$returnMessage)
      )
      return(response_json)
    },
    error = function(e) {
      log_message("error", sprintf("接口调用失败: %s", e$message))
      return(NULL)
    }
  )
}

# 获取医疗机构ID
get_hospital_id <- function(current_env) {
  log_message("info", "开始获取医疗机构ID...")

  params <- list(
    loginname = config$login_name,
    loginpwd = config$login_pwd
  )

  url <- paste0(current_env$base_url, "/HosGetHosInformation")
  json_body <- toJSON(params, auto_unbox = TRUE)

  response <- POST(
    url = url,
    body = json_body,
    add_headers("Content-Type" = "application/json; charset=utf-8"),
    encode = "json"
  )

  if (status_code(response) == 200) {
    response_json <- fromJSON(content(response, "text", encoding = "UTF-8"))

    if (response_json$returnCode == "0001") {
      config <<- modifyList(config, list(hospital_id = response_json$data))
      log_message("info", sprintf("成功获取医疗机构ID"))
      return(TRUE)
    } else {
      log_message(
        "error",
        sprintf("获取医疗机构ID失败: %s", response_json$returnMessage)
      )
      return(FALSE)
    }
  } else {
    log_message("error", sprintf("HTTP请求失败: %s", status_code(response)))
    return(FALSE)
  }
}

# 获取查询订单
get_order_shipment_return_info <- function(
  ordshpretno = NULL,
  ordtype = NULL,
  start_date = NULL,
  end_date = NULL,
  down_flag = NULL,
  current_env
) {
  log_message("info", "开始获取订单、随货单、退货单信息...")

  params <- list(
    ordshpretno = ordshpretno,
    ordtype = ordtype,
    starttime = start_date,
    endtime = end_date,
    downflag = down_flag
  )

  # 移除NULL参数
  params <- params[!sapply(params, is.null)]

  result <- call_api("HosOrderShpRetCheck", params, current_env)

  if (!is.null(result) && !is.null(result$data)) {
    orders_data <- result$data

    if (is.list(orders_data) && length(orders_data) > 0) {
      log_message(
        "info",
        sprintf("成功获取 %d 条订单/随货单/退货单信息", length(orders_data))
      )

      # 展开订单明细并转换为数据框
      orders_df <- orders_data %>%
        dplyr::select(-ordorderid) %>%
        tidyr::unnest(orderDetailList) %>%
        dplyr::select(
          订单ID = ordorderid,
          订单编号 = ordorderno,
          医疗机构名称 = hospitalname,
          配送企业名称 = entname,
          订单创建时间 = createtime,
          紧急程度 = isurgency,
          制单人用户名 = createusername,
          订单状态 = ordorderstatus,
          企业反馈时间 = freebacktime,
          收货地址 = receiveaddress,
          订单采购备注 = remarks,
          订单明细ID = ordorderdetailid,
          药品编码 = sysmedicineid,
          药品通用名 = medcurrencynames,
          商品名 = pruductname,
          剂型 = medpreparation,
          规格 = medtypespec,
          包装 = packagspecifications,
          生产企业 = productiveentname,
          批准文号 = approvedno,
          单位 = unit,
          采购数量 = purquantity,
          采购价格 = purprice,
          企业反馈数量 = quantityfeedback,
          企业反馈备注 = entremark,
          医院反馈状态 = hospitalfeedback,
          医院反馈确认备注 = hospitalremark,
          订单明细采购备注 = remark
        ) %>% distinct()

      return(orders_df)
    } else {
      log_message("info", "没有找到订单/随货单/退货单信息")
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# 获取常用药品信息
get_medication_info <- function(start_index = 1, end_index = 100, current_env) {
  log_message("info", "开始获取常用药品信息...")

  params <- list(
    startindex = start_index,
    endindex = end_index
  )

  result <- call_api("HosGetMedInformation", params, current_env)

  if (!is.null(result) && !is.null(result$data)) {
    medications <- result$data

    if (is.list(medications) && length(medications) > 0) {
      log_message("info", sprintf("成功获取 %d 条药品信息", nrow(medications)))

      # 转换为数据框
      meds_df <- medications %>%
        transmute(
          药品编码 = sysmedicineid,
          通用名 = medcurrencynames,
          剂型 = medpreparation,
          规格 = medtypespec,
          包装 = packagspecifications,
          生产企业 = productiveentname,
          采购价格 = medprice,
          批准文号 = approvedno,
          药品类别 = medtype,
          stringsAsFactors = FALSE
        )

      return(meds_df)
    } else {
      log_message("info", "没有找到药品信息")
      return(NULL)
    }
  } else {
    return(NULL)
  }
}

# 定义UI
ui <- fluidPage(
  titlePanel("网采数据查询系统"),
  
  tabsetPanel(
    tabPanel("订单查询", 
      # 上下布局，将输入控件放在上方，表格放在下方
      fluidRow(
        column(12,
          wellPanel(
            h4("查询条件"),
            fluidRow(
              column(3, textInput("username", "用户名:", "")),
              column(3, passwordInput("password", "密码:", "")),
              column(3, 
                radioButtons("env", "环境选择:", 
                           choices = list("测试环境" = "test", 
                                         "生产环境" = "prod"), 
                           selected = "prod")
              ),
              column(3, 
                dateRangeInput("date_range", "选择时间范围:",
                              start = Sys.Date() - 15,  # 默认为当前时间的前一个月
                              end = Sys.Date())
              )
            ),
            fluidRow(
              column(12,
                actionButton("confirm", "确认查询", 
                           class = "btn-primary", 
                           style = "margin-top: 10px; width: 100%;")
              )
            )
          )
        )
      ),
      
      # 表格输出区域
      fluidRow(
        column(12,
          h3("企业反馈数量少于采购数量的品种"),
          DT::dataTableOutput("results_table")
        )
      )
    ),
    
    tabPanel("医保核对",
      # 上下布局，将输入控件放在上方，表格放在下方
      fluidRow(
        column(12,
          wellPanel(
            h4("核对条件"),
            fluidRow(
              column(3, textInput("username2", "用户名:", "")),
              column(3, passwordInput("password2", "密码:", "")),
              column(3,
                radioButtons("env2", "环境选择:", 
                           choices = list("测试环境" = "test", 
                                         "生产环境" = "prod"), 
                           selected = "prod")
              ),
              column(3,
                fileInput("upload_file", "上传Excel文件", 
                         accept = c(".xlsx", ".xls"))
              )
            ),
            fluidRow(
              column(12,
                actionButton("check_medical", "获取常用药目录并核对", 
                           class = "btn-primary", 
                           style = "margin-top: 10px; width: 100%;"),
                
                br(), br(),
                
                helpText("上传的Excel文件应包含'药品编码'列，用于与医保目录进行比对")
              )
            )
          )
        )
      ),
      
      # 表格输出区域
      fluidRow(
        column(12,
          h3("常用药目录与医保核对结果"),
          DT::dataTableOutput("medical_check_table")
        )
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  # 存储查询结果
  query_results <- reactiveValues(data = NULL)
  medical_data <- reactiveValues(common_meds = NULL, uploaded_data = NULL)
  
  # 确认按钮点击事件
  observeEvent(input$confirm, {
    # 验证用户名和密码不为空
    if (is.null(input$username) || input$username == "" || 
        is.null(input$password) || input$password == "") {
      showNotification("用户名和密码不能为空！", type = "error")
      return()
    }
    
    # 更新配置
    config <<- modifyList(config, 
                         list(login_name = input$username, 
                              login_pwd = input$password))
    
    # 获取当前环境配置
    current_env <- env_configs[[input$env]]
    
    # 获取医院ID
    success <- get_hospital_id(current_env)
    if (!success) {
      showNotification("获取医院ID失败！", type = "error")
      return()
    }
    
    # 获取订单信息
    withProgress(message = '正在查询数据...', value = 0.5, {
      order_data <- get_order_shipment_return_info(
        ordtype = "1", # 1订单 2随货单 3退货单
        start_date = format(input$date_range[1], "%Y-%m-%d"),
        end_date = format(input$date_range[2], "%Y-%m-%d"),
        current_env = current_env
      )
    })
    
    if (is.null(order_data)) {
      showNotification("未获取到订单数据！", type = "error")
      query_results$data <- NULL
      return()
    }
    
    # 筛选企业反馈数量少于采购数量的记录（排除采购数量为0的情况）
    filtered_data <- order_data %>%
      mutate(
        采购数量 = as.numeric(采购数量),
        企业反馈数量 = as.numeric(企业反馈数量),
        反馈率 = ifelse(is.na(企业反馈数量) | 采购数量 == 0, 0, 企业反馈数量 / 采购数量)
      ) %>%
      filter(采购数量 > 0 & (企业反馈数量 < 采购数量 | is.na(企业反馈数量) | is.null(企业反馈数量))) %>%
      distinct()
    
    if (nrow(filtered_data) == 0) {
      showNotification("未找到企业反馈数量少于采购数量的记录！", type = "warning")
    } else {
      showNotification(paste("找到", nrow(filtered_data), "条企业反馈数量少于采购数量的记录"), 
                      type = "message")
    }
    
    # 更新查询结果
    query_results$data <- filtered_data
  })
  
  # 输出表格
  output$results_table <- DT::renderDataTable({
    if (is.null(query_results$data) || nrow(query_results$data) == 0) {
      return(data.frame(提示 = "暂无数据"))
    }
    
    # 添加反馈率列并处理数据
    display_data <- query_results$data %>%
      mutate(
        采购数量 = as.numeric(采购数量),
        企业反馈数量 = as.numeric(企业反馈数量),
        订单创建时间 = format(as.Date(订单创建时间, format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d"), # 格式化日期
        反馈率 = case_when(
          is.na(企业反馈数量) | 采购数量 == 0 ~ "0%",
          TRUE ~ paste0(round((企业反馈数量 / 采购数量) * 100, 2), "%")
        )
      ) %>%
      select(
        时间 = 订单创建时间,
        药品名 = 药品通用名,
        规格,
        生产企业,
        单位,
        应采 = 采购数量,
        实发 = 企业反馈数量,
        反馈率,
        配送 = 配送企业名称,
        备注 = 企业反馈备注
      ) %>% 
      arrange(
        desc(时间),
        反馈率
      )
    
    datatable(
      display_data,
      extensions = 'Buttons', # 启用按钮扩展
      options = list(
        pageLength = 500,
        lengthMenu = list(c(15, 30, 50, -1), c("15", "30", "50", "全部")),
        scrollX = TRUE,
        dom = 'Bfrtip', # B 代表按钮
        buttons = c('excel') # 添加 Excel 按钮
      ),
      rownames = FALSE
    )
  })
  
  # 医保核对功能
  observeEvent(input$check_medical, {
    # 验证用户名和密码不为空
    if (is.null(input$username2) || input$username2 == "" || 
        is.null(input$password2) || input$password2 == "") {
      showNotification("用户名和密码不能为空！", type = "error")
      return()
    }
    
    if (is.null(input$upload_file)) {
      showNotification("请先上传Excel文件！", type = "error")
      return()
    }
    
    # 更新配置
    config <<- modifyList(config, 
                         list(login_name = input$username2, 
                              login_pwd = input$password2))
    
    # 获取当前环境配置
    current_env <- env_configs[[input$env2]]
    
    # 获取医院ID
    success <- get_hospital_id(current_env)
    if (!success) {
      showNotification("获取医院ID失败！", type = "error")
      return()
    }
    
    # 读取上传的Excel文件
    tryCatch({
      common_cols_need <- c("类型","医保目录名称","开始日期",
                            "注册剂型","注册规格",
                            # "标准化最小单位", "标准化规格","包装规格","包装数量","规格数",
                            "包装单位","规格单位", "基本药物标志",
                            "限制使用范围", "门特药品使用范围","批准文号","生产企业名称","药品编码",
                            "医保目录定价上限金额(三级)","自付比例","调整原因")
      ## 读取医保调整目录
      data_new = rio::import(input$upload_file$datapath,
                            sheet = "新增",skip =2,
                            # 设置类型检测参数，避免自动类型转换
                            stringsAsFactors = FALSE) %>% 
        filter(!is.na(`药品编码`), !str_detect(`药品编码`,"-")) %>% 
        mutate(类型 = "新增", 调整原因 = NA) %>%
        dplyr::select(any_of(common_cols_need))

      data_change = rio::import(input$upload_file$datapath,
                                sheet = "调整",skip =2,
                                stringsAsFactors = FALSE)  %>% 
        filter(!is.na(`药品编码`), !str_detect(`药品编码`,"-")) %>% 
        mutate(类型 = "调整") %>%
        filter(`数据类型（1：历史数据2：结束节点数据3：新数据）` == "新数据") %>%
        dplyr::select(any_of(common_cols_need))

      data_drop = rio::import(input$upload_file$datapath,
                              sheet = "停用",skip =2,
                              stringsAsFactors = FALSE) %>% 
        mutate(`备注` = `备注...65`) %>% select(-`备注...65`) %>% 
        filter(!is.na(`药品编码`), !str_detect(`药品编码`,"-")) %>% 
        mutate(类型 = "停用", 调整原因 = NA) %>%
        filter(`数据类型（1：历史数据2：结束节点数据3：新增数据）` == "结束节点数据") %>%
        dplyr::select(any_of(common_cols_need))

      uploaded_data <- rbind(data_new, data_change, data_drop)


      medical_data$uploaded_data <- uploaded_data
      
      if (!"药品编码" %in% colnames(uploaded_data)) {
        showNotification("上传的文件中没有找到'药品编码'列！", type = "error")
        return()
      }
    }, error = function(e) {
      showNotification(paste("读取Excel文件失败:", e$message), type = "error")
      return()
    })
    
    # 获取常用药品信息
    withProgress(message = '正在获取常用药目录...', value = 0.5, {
      common_meds <- get_medication_info(1, 5000, current_env)
    })
    
    if (is.null(common_meds)) {
      showNotification("未获取到常用药品信息！", type = "error")
      return()
    }
    
    medical_data$common_meds <- common_meds
    
    # 进行医保核对
    merged_data <- medical_data$uploaded_data %>%
      filter(药品编码 %in% common_meds$药品编码)
    # 更新结果
    medical_data$check_result <- merged_data
    
    showNotification(paste("医保核对完成，共处理", nrow(merged_data), "条记录"), 
                    type = "message")
  })
  
  # 输出医保核对表格
  output$medical_check_table <- DT::renderDataTable({
    if (is.null(medical_data$check_result) || nrow(medical_data$check_result) == 0) {
      return(data.frame(提示 = "暂无医保核对数据"))
    }
    
    # 添加是否匹配的列
    display_data <- medical_data$check_result
    
    datatable(
      display_data,
      extensions = 'Buttons', # 启用按钮扩展
      options = list(
        pageLength = 500,
        lengthMenu = list(c(15, 30, 50, -1), c("15", "30", "50", "全部")),
        scrollX = TRUE,
        dom = 'Bfrtip', # B 代表按钮
        buttons = c('excel') # 添加 Excel 按钮
      ),
      rownames = FALSE
    )
  })
}

# 运行应用程序
shinyApp(ui = ui, server = server)