#' Shiny App for efficient determination of the size of experiments in Balanced design of experiments
#'
#' Shiny App for efficient determination of the size of experiments in Balanced design of experiments
#' @usage BDEsizeApp()
#' @export
#' @examples
#' #BDEsizeApp()
BDEsizeApp<-function() {
if (interactive()) {
  full_list1  <- new.env()

  Size_app=shiny::shinyApp(
  ui=shiny::fluidPage( shiny::tagList(

    shiny::navbarPage(
      "Design options",
      shiny::tabPanel("Factorial Design",
                      shiny::sidebarPanel(
                        shiny::numericInput('nf','Number of Factor',2,min=1,max=10),
                        shiny::textInput("fl", 'Factor Levels', value = "2,2"),
                        shiny::checkboxGroupInput("checkGroup", "Order",
                                    choices = list("Main" = 1, "Two-way Interactions" = 2),
                                    selected = 1),
                        shiny::radioButtons(
                      "delta_type", "Type of effect size", inline = TRUE,
                      c("SD" = 1, "Range of effect" =2)
                              ),
                      shiny::conditionalPanel("input.delta_type==1",
                                              shiny::numericInput('de1','SD(Main)',1,min=0,max=100),
                                              shiny::numericInput('de2','SD(Interaction)',1,min=0,max=100),
                                              shiny::numericInput('de3','SD(Noise)',1,min=0,max=100)
                                      )  ,
                      shiny::conditionalPanel("input.delta_type==2",
                                              shiny::numericInput('de11','Range of effect(Main)',1,min=0,max=100),
                                              shiny::numericInput('de12','Range of effect(Interaction)',1,min=0,max=100),
                                              shiny::numericInput('de13','SD(Noise)',1,min=0,max=100)
                                     )  ,
                      shiny::numericInput('a','Type I error',0.05,min=0,max=1),
                      shiny::numericInput('b','Power',0.8,min=0,max=1),
                      shinyalert::useShinyalert(),
                      shiny::actionButton("do", "Click Me" , shiny::icon("paper-plane"),
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),

               shiny::mainPanel(
                 shiny::tabsetPanel(
                   shiny::tabPanel("Result",
                                   shiny::h4('Model'),
                            shiny::verbatimTextOutput("list1" ),
                            shiny::h4('Sample Size'),
                            shiny::verbatimTextOutput("Size1"),
                            shiny::h4('Detectable effect size'),
                            shiny::verbatimTextOutput("Size2")
                            ),
                   shiny::tabPanel("Delta vs Power Plot",
                                   shiny::sidebarLayout(
                                     shiny::sidebarPanel(
                                       shiny::selectInput("plot_order", "List of factors:",
                                            choices=c(toupper(letters)[1:2],"ALL"))
                                ),
                                shiny::mainPanel(shiny::plotOutput("Delta_graph") ,
                                                 shiny::tableOutput("values")
                                      )
                            )
                            ),
                   shiny::tabPanel("Size vs Delta Plot",  shiny::plotOutput("Size_graph")) ,
                   shiny::tabPanel("Size vs Power Plot",
                                   shiny::sidebarLayout(
                                     shiny::sidebarPanel(
                                       shiny::selectInput("plot_delta", "Delta",
                                            choices=c(0.5,1,1.5,2.0)
                                            )
                                ),
                                shiny::mainPanel(shiny::plotOutput("power_graph")
                                        )
                              )
                            )
                   )
                 )
               ),
      shiny::tabPanel("Fractional FD",
                      shiny::sidebarPanel(
                        shiny::numericInput('nf2','Number of 2 level Factor',2,min=1,max=10),
                        shiny::numericInput('fr2','Fraction p (eg:2^(k-p))',1,min=1,max=10),
                        shiny::checkboxGroupInput("checkGroup2", "Order",
                           choices = list("Main" = 1, "Two-way Interactions" = 2),
                           selected = 1),
                        shiny::radioButtons(
                   "delta_type2", "Type of effect size", inline = TRUE,
                   c("SD" = 1, "Range of effect" =2)
                   ),
                   shiny::conditionalPanel("input.delta_type2==1",
                                           shiny::numericInput('de1_2','SD(Main)',1,min=0,max=100),
                                           shiny::numericInput('de2_2','SD(Interaction)',1,min=0,max=100),
                                           shiny::numericInput('de3_2','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny::conditionalPanel("input.delta_type2==2",
                                           shiny::numericInput('de11_2','Range of effect(Main)',1,min=0,max=100),
                                           shiny::numericInput('de12_2','Range of effect(Interaction)',1,min=0,max=100),
                                           shiny::numericInput('de13_2','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny::numericInput('a2','Type I error',0.05,min=0,max=1),
                   shiny::numericInput('b2','Power',0.8,min=0,max=1),
                   shinyalert::useShinyalert(),
                   shiny::actionButton("do2", "Click Me" , shiny::icon("paper-plane"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 ),
                 shiny::mainPanel(
                   shiny::tabsetPanel(
                     shiny::tabPanel("Result",
                                     shiny::h4('Model'),
                                     shiny::verbatimTextOutput("list1_2" ),
                                     shiny::h4('Sample Size'),
                                     shiny::verbatimTextOutput("Size12"),
                                     shiny::h4('Detectable effect size'),
                                     shiny::verbatimTextOutput("Size22")
                   ),
                   shiny::tabPanel("Delta vs Power Plot",
                                   shiny::sidebarLayout(
                                     shiny::sidebarPanel(
                                       shiny::selectInput("plot_order2", "List of factors:",
                                   choices=c(toupper(letters)[1:2],"ALL"))
                       ),
                       shiny::mainPanel(shiny::plotOutput("Delta_graph2"),
                                        shiny::tableOutput("values2")
                               )
                     )
                   ),
                   shiny::tabPanel("Size vs Delta Plot",  shiny::plotOutput("Size_graph2")) ,
                   shiny::tabPanel("Size vs Power Plot",
                                   shiny::sidebarLayout(
                                     shiny::sidebarPanel(
                                       shiny::selectInput("plot_delta2", "Delta",
                                   choices=c(0.5,1,1.5,2.0))
                       ),
                       shiny::mainPanel( shiny::plotOutput("power_graph2"))
                     )
                   )
          )
        )
      ),
      shiny::tabPanel("Randomized Block",
                      shiny::sidebarPanel(
                        shiny::numericInput('nf3','Number of Factor',2,min=1,max=10),
                        shiny::textInput("fl3", 'Factor Levels', value = "2,2"),
                        shiny::checkboxGroupInput("checkGroup3", "Order",
                                    choices = list("Main" = 1, "Two-way Interactions" = 2),
                                    selected = 1),
                        shiny::radioButtons(
                   "delta_type3", "Type of effect size", inline = TRUE,
                   c("SD" = 1, "Range of effect" =2)
                   ),
                   shiny::conditionalPanel("input.delta_type3==1",
                                           shiny::numericInput('de1_3','SD(Main)',1,min=0,max=100),
                                           shiny::numericInput('de2_3','SD(Interaction)',1,min=0,max=100),
                                           shiny::numericInput('de3_3','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny:: conditionalPanel("input.delta_type3==2",
                                            shiny::numericInput('de11_3','Range of effect(Main)',1,min=0,max=100),
                                            shiny::numericInput('de12_3','Range of effect(Interaction)',1,min=0,max=100),
                                            shiny::numericInput('de13_3','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny::numericInput('a3','Type I error',0.05,min=0,max=1),
                   shiny::numericInput('b3','Power',0.8,min=0,max=1),
                   shinyalert::useShinyalert(),
                   shiny::actionButton("do3", "Click Me" , shiny::icon("paper-plane"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 ),
                 shiny::mainPanel(
                   shiny::tabsetPanel(
                     shiny::tabPanel("Result",
                                     shiny::h4('Model'),
                                     shiny::verbatimTextOutput("list1_3" ),
                                     shiny::h4('Sample Size'),
                                     shiny::verbatimTextOutput("Size1_3"),
                                     shiny::h4('Detectable effect size'),
                                     shiny::verbatimTextOutput("Size2_3")
                            ),
                     shiny::tabPanel("Delta vs Power Plot",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::selectInput("plot_order3", "List of factors:",
                                            choices=c(toupper(letters)[1:2],"ALL"))
                                ),
                                shiny::mainPanel(shiny::plotOutput("Delta_graph3"),
                                                 shiny::tableOutput("values3")
                                        )
                              )
                            ) ,
                     shiny::tabPanel("Size vs Delta Plot",  shiny::plotOutput("Size_graph3")) ,
                     shiny::tabPanel("Size vs Power Plot",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::selectInput("plot_delta3", "Delta",
                                            choices=c(0.5,1,1.5,2.0))
                                ),
                                shiny::mainPanel(shiny::plotOutput("power_graph3"))
                              )
                            )
                   )
                 )
               ),
      shiny::tabPanel("Split-Plot Design",
                      shiny::sidebarPanel(
                        shiny::numericInput('wf','Number of Whole Factor',2,min=1,max=10),
                        shiny::textInput("wfl", 'Whole Factor Levels', value = "2,2"),
                        shiny::numericInput('sf','Number of Split Factor',2,min=1,max=10),
                        shiny::textInput("sfl", 'Split Factor Levels', value = "2,2"),
                        shiny::checkboxGroupInput("checkGroup4", "Order",
                                    choices = list("Main" = 1, "Two-way Interactions" = 2),
                                    selected = 1),
                        shiny::radioButtons(
                   "delta_type4", "Type of effect size", inline = TRUE,
                   c("SD" = 1, "Range of effect" =2)
                   ),
                   shiny::conditionalPanel("input.delta_type4==1",
                                           shiny::numericInput('de1_4','SD(Main)',1,min=0,max=100),
                                           shiny::numericInput('de2_4','SD(Interaction)',1,min=0,max=100),
                                           shiny::numericInput('de3_4','SD(Whole Noise)',1,min=0,max=100),
                                           shiny::numericInput('de4_4','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny::conditionalPanel("input.delta_type4==2",
                                           shiny::numericInput('de11_4','Range of effect(Main)',1,min=0,max=100),
                                           shiny::numericInput('de12_4','Range of effect(Interaction)',1,min=0,max=100),
                                           shiny::numericInput('de13_4','SD(Whole Noise)',1,min=0,max=100),
                                           shiny::numericInput('de14_4','SD(Noise)',1,min=0,max=100)
                                  )  ,
                   shiny::numericInput('a4','Type I error',0.05,min=0,max=1),
                   shiny::numericInput('b4','Power',0.8,min=0,max=1),
                   shinyalert::useShinyalert(),
                   shiny::actionButton("do4", "Click Me" , shiny::icon("paper-plane"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 ),
                 shiny::mainPanel(
                   shiny::tabsetPanel(
                     shiny::tabPanel("Result",
                                     shiny::h4('Model'),
                                     shiny::verbatimTextOutput("list1_4" ),
                                     shiny::h4('Sample Size'),
                                     shiny::verbatimTextOutput("Size1_4"),
                                     shiny::h4('Detectable effect size'),
                                     shiny::verbatimTextOutput("Size2_4")
                            ),
                     shiny::tabPanel("Delta vs Power Plot",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::selectInput("plot_order4", "List of factors:",
                                            choices=c(toupper(letters)[1:2],"ALL"))
                                ),
                                shiny::mainPanel(shiny::plotOutput("Delta_graph4"),
                                                 shiny::tableOutput("values4")
                                        )
                              )
                            ) ,
                     shiny::tabPanel("Size vs Delta Plot",  shiny::plotOutput("Size_graph4")) ,
                     shiny::tabPanel("Size vs Power Plot",
                                     shiny::sidebarLayout(
                                       shiny::sidebarPanel(
                                         shiny::selectInput("plot_delta4", "Delta",
                                            choices=c(0.5,1,1.5,2.0))
                                ),
                                shiny::mainPanel(shiny::plotOutput("power_graph4"))
                              )
                            )
                   )
                 )
               )
      )
    )
    ), #end ui


  server <- function(input, output,session)
    {
    shiny::observeEvent(input$do,
                 {
                   if ( input$nf==1 &  max(input$checkGroup)==2)
                   {
                     shinyalert::shinyalert("Error!", "The number of factor is just one! Two-way interactions can't be estimated.", type = "error")
                   }

                   else{
    list_tmp<-sizelist(input$nf,max(input$checkGroup))
    full_list<-list_tmp$full_list
    full_list1$full<-c(full_list,"ALL")
    shiny::updateSelectInput(session,"plot_order",choices=full_list1$full)

     A<-Size.Full(input$nf, as.numeric(unlist(strsplit(input$fl,","))),  delta_type=input$delta_type,order=max(input$checkGroup), delta=ifelse(rep(input$delta_type==1,3),c(input$de1,input$de2,input$de3),c(input$de11,input$de12,input$de13)), beta=1-input$b, alpha=input$a)
    if(length(A)==1)
    {
      shinyalert::shinyalert("Error!", "Sample size exceeds 1,000.", type = "error")
    }
     else {
    output$Size1<-shiny::renderText({A$n})
    output$Size2<-shiny::renderText({A$Delta})
    output$list1<-shiny::renderText({A$model})

   #####Graph
    Delta.graph<-function (factor, factor.lev,delta_type, order,  delta , alpha=0.05, beta=0.2,plot_factor)
    {
      (n.choose <- A$n);
      (Delta.choose <- data.frame(t(A$Delta)))
      power <- round(seq(0,1,length.out=1001),3)
      start <- ifelse((n.choose-1)<=2, 2, n.choose-2)
      Delta <- array(0,c(1000,n.choose-start+1, ncol(Delta.choose)))
      delta.pwr <- matrix(0,n.choose-start+1, ncol(Delta.choose))

      temp_v<-list()
      temp_c<-list()
      temp_denom<-list()
      temp_n<-list()

      k<-1
      if( order==2)
      {
        v_flag<-c(rep(0,factor), rep(1,factor*(factor-1)/2))

      }
      else if (order==1){
        v_flag<-rep(0,factor+factor*(factor-1)/2)
      }

      for (n in start:(n.choose)) {
        v1=n-1
        if (order==1){
          v <- factor.lev-1
          c <- prod(factor.lev)*n/factor.lev
        }
        else if (order==2) {
          v <- (factor.lev-1)%*%t(factor.lev-1)
          v <- c(factor.lev-1,v[upper.tri(v, diag=FALSE)])
          c <- prod(factor.lev)*n/c(factor.lev, (factor.lev%*%t(factor.lev))[upper.tri((factor.lev)%*%t(factor.lev), diag=FALSE)])
        }
        v.denom <-  prod(factor.lev)*n-1-sum(v)

        for (j in 1: length(v) ){

          for (ind in 1: 1000){
                      if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
              (Delta[ind,(n-start+1),j] <-  fsize(alpha, 1-power[ind], v[j], v.denom, c[j], delta_type ,v_flag[j]))
            else (Delta[ind,(n-start+1),j]<-NA)
          }
        }
        temp_n[[k]]<-n
        temp_v[[k]]<-v
        temp_c[[k]]<-c
        temp_denom[[k]]<-v.denom
        k<-k+1
      }

      x<-seq(1:length(full_list1$full))
      i<-x[full_list1$full==plot_factor]

      if(i==max(x))
      {
        for(j in 1: length(v))
        {
          tmp1<-rep(full_list1$full[[j]],1000)
          if ( j==1)
          {
            factor_type<-tmp1
            Delta1<-Delta[,(n.choose -start+1),j]
            power1 <-power[1:1000]
          }
          else if (j>1)
          {
            factor_type<-t(cbind(t(factor_type),t(tmp1)))
            Delta1<-t(cbind(t(Delta1),t(Delta[,(n.choose -start+1),j])))
            power1 <-t(cbind(t(power1),t(power[1:1000])))
          }

        }
        data<-data.frame(factor_type, Delta1, power1)
        l<-c(unique(factor_type))
        data$factor_type<- factor(data$factor_type, levels = l)

        gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
          ggplot2::geom_line(size=1.5)   +
          ggplot2::labs(title = "Delta vs Power") +
          ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
          ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
          ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
          ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

        values<-NULL
      }

      else if(i<max(x))
      {


        for( j in 1: (n.choose -start+1))
        {
          tmp1<-rep(start+j-1,1000)
          if(j==1)
          {
            size1<-tmp1
            Delta1<-Delta[,j,i]
            power1 <-power[1:1000]
          }

          else
          {
            size1<-t(cbind(t(size1),t(tmp1)))
            Delta1<-t(cbind(t(Delta1),t(Delta[,j,i])))
            power1 <-t(cbind(t(power1),t(power[1:1000])))
          }
        }

        data<-data.frame(size1, Delta1, power1)
        l<-c(unique(size1))
        data$size1<- factor(data$size1, levels = l)

        gr<- ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=size1 , shape=size1,color=size1)) +
          ggplot2::geom_line(size=1.5)   +
          ggplot2::labs(title =ifelse(plot_factor=="ALL",paste0("Delta vs Power"),ifelse( delta_type ==1,paste0("SD(",plot_factor,")/SD(noise) vs Power"),paste0("Range(",plot_factor,")/SD(noise) vs Power")))) +
          ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
          ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
          ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
          ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

        for ( j in 1:(n.choose -start+1) )
        {
          if(j==1)
          {
          RR= rep(temp_n[[j]],4)
          Ddelta<-  c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                      round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                      "1.0","1.5")
          Ppower<- c("0.8","0.9",
                     round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                        ifelse( v_flag[i] ==1 ,
                                                                                                                                (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                     round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                        ifelse( v_flag[i] ==1,
                                                                                                                                (1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))
          }
          else
          {
            RR<-t(cbind(t(RR),t(rep(temp_n[[j]],4)) ))
            Ddelta<-t(cbind(t(Ddelta),t( c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                                           round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                                           "1.0","1.5")) ))

            Ppower<-t(cbind(t(Ppower),t( c("0.8","0.9",
                                           round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                              ifelse( v_flag[i] ==1 ,
                                                                                                                                                      (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                                           round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                              ifelse( v_flag[i] ==1,
                                                                                                                                                      (1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))) ))
          }
        }
        values<-data.frame(R=RR, Delta=Ddelta, Power=Ppower,
                           stringsAsFactors = FALSE)

      }
      return( list(gr=gr, value=values ))
    }
       output$Delta_graph  <- shiny::renderPlot({Delta.graph(input$nf, as.numeric(unlist(strsplit(input$fl,","))),  order=max(input$checkGroup), delta_type=input$delta_type,delta =ifelse(rep(input$delta_type==1,3),c(input$de1,input$de2,input$de3),c(input$de11,input$de12,input$de13)), beta=1-input$b, alpha=input$a,plot_factor=input$plot_order)$gr})
    output$values  <- shiny::renderTable({Delta.graph(input$nf, as.numeric(unlist(strsplit(input$fl,","))),  order=max(input$checkGroup), delta_type=input$delta_type,delta =ifelse(rep(input$delta_type==1,3),c(input$de1,input$de2,input$de3),c(input$de11,input$de12,input$de13)), beta=1-input$b, alpha=input$a,plot_factor=input$plot_order)$value})

   output$Size_graph<-shiny::renderPlot({
     plots.Full(factor=input$nf, factor.lev=as.numeric(unlist(strsplit(input$fl,","))),order=max(input$checkGroup) , delta_type=input$delta_type , delta=ifelse(rep(input$delta_type==1,3),c(input$de1,input$de2,input$de3),c(input$de11,input$de12,input$de13)), deltao=1 , alpha=input$a, beta=1-input$b,type=2)

     }
     )

   output$power_graph<-shiny::renderPlot({
     plots.Full(factor=input$nf, factor.lev=as.numeric(unlist(strsplit(input$fl,","))),order=max(input$checkGroup) , delta_type=input$delta_type , delta=ifelse(rep(input$delta_type==1,3),c(input$de1,input$de2,input$de3),c(input$de11,input$de12,input$de13)),deltao=as.numeric(input$plot_delta) , alpha=input$a, beta=1-input$b,type=3)

     }
   )
     }
                   }
   }
   )

    shiny::observeEvent(input$do2,
                 {

      if (2^(input$nf2-input$fr2)-1 -(input$nf2+input$nf2*(input$nf2-1)/2)<0 & max(input$checkGroup2)==2)
      {
        shinyalert::shinyalert("Error!", "Two-way interactions can't be estimated.", type = "error")
      }

      else{

      if(max(input$checkGroup2)==1)
      {
        full_list1$fr<-c("Main")
      }
      else if(max(input$checkGroup2)==2)
      {
        full_list1$fr<-c("Main","Two-way interactions","ALL")
        }

        shiny::updateSelectInput(session,"plot_order2",choices=full_list1$fr)

      B<-Size.2levFr(input$nf2, input$fr2, order=max(input$checkGroup2),delta_type=input$delta_type2,delta=ifelse(rep(input$delta_type2==1,3),c(input$de1_2,input$de2_2,input$de3_2),c(input$de11_2,input$de12_2,input$de13_2)), beta=1-input$b2, alpha=input$a2)
      if(length(B)==1)
      {
        shinyalert::shinyalert("Error!", "Sample size exceeds 1,000.", type = "error")
      }
      else {
      output$Size12<-shiny::renderText({B$n})
      output$Size22<-shiny::renderText({B$Delta})
      output$list1_2<-shiny::renderText({B$model})

       Delta.graph2<-function (factor, fraction,delta_type, order,  delta , alpha=0.05, beta=0.2,plot_factor)
       {
        (n.choose <- B$n);
        (Delta.choose <- data.frame(t(B$Delta)))
        power <- round(seq(0,1,length.out=1001),3)
        start <- ifelse((n.choose-1)<=2, 2, n.choose-2)
        Delta <- array(0,c(1000,n.choose-start+1, ncol(Delta.choose)))
        delta.pwr <- matrix(0,n.choose-start+1, ncol(Delta.choose))

        factor.lev<-2
        temp_v<-list()
        temp_c<-list()
        temp_denom<-list()
        temp_n<-list()
        k<-1

        for (n in start:(n.choose)) {
        v1=n-1
        if (order==1){
          v <-  (rep(2,factor)-1)
          c <- (prod(rep(2,factor))/rep(2,fraction) )*n/rep(2,factor)
          v.denom<- 2^(factor-fraction)*n-1-factor
          for (ind in 1: 1000){
                      if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
              (Delta[ind,(n-start+1),1]<- fsize(alpha, 1-power[ind], v[1], v.denom, c[1],delta_type,0))
            else (Delta[ind,(n-start+1),1]<-NA)
            }
          temp_v[[k]]<-v[1]
          temp_c[[k]]<-c[1]
          }
        else if (order==2) {
          v <-(rep(2,factor)-1)%*%t(rep(2,factor)-1)
          v <- c(rep(2,factor)-1,v[upper.tri(v, diag=FALSE)])
          c <- (prod(rep(2,factor))/rep(2,fraction) )*n/c(rep(2,factor), (rep(2,factor)%*%t(rep(2,factor)))[upper.tri((rep(2,factor))%*%t(rep(2,factor)), diag=FALSE)])
          v.denom<- 2^(factor-fraction)*n-1-factor-factor*(factor-1)/2
          for (ind in 1: 1000){
                      if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
            {
              (Delta[ind,(n-start+1),1]<- fsize(alpha, 1-power[ind], v[1], v.denom, c[1],delta_type,0))
              (Delta[ind,(n-start+1),2] <- fsize(alpha, 1-power[ind], v[factor+1], v.denom, c[factor+1],delta_type,1))
            }
            else
              {
                (Delta[ind,(n-start+1),1]<-NA)
                (Delta[ind,(n-start+1),2]<-NA)
              }
          }
          temp_v[[k]]<-v[c(1,factor+1)]
          temp_c[[k]]<-c[c(1,factor+1)]
          }
        temp_n[[k]]<-n
        temp_denom[[k]]<-v.denom
        k<-k+1
        }

        x<-seq(1:length(full_list1$fr))
        i<-x[full_list1$fr== plot_factor ]
        if(i==max(x) & length(x)==3)
        {

          for(j in 1: 2)
          {
            tmp1<-rep(full_list1$fr[[j]],1000)
            if ( j==1)
            {
              factor_type<-tmp1
              Delta1<-Delta[,(n.choose -start+1),j]
              power1 <-power[1:1000]
            }
            else if (j>1)
            {
              factor_type<-t(cbind(t(factor_type),t(tmp1)))
              Delta1<-t(cbind(t(Delta1),t(Delta[,(n.choose -start+1),j])))
              power1 <-t(cbind(t(power1),t(power[1:1000])))
            }

          }
          data<-data.frame(factor_type, Delta1, power1)
          l<-c(unique(factor_type))
          data$factor_type<- factor(data$factor_type, levels = l)

          gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
            ggplot2::geom_line(size=1.5)   +
            ggplot2::labs(title = "Delta vs Power") +
            ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
            ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
            ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
            ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

          values<-NULL
          }
        else if (i<max(x) || length(x)==1)
        {

          for( j in 1: (n.choose -start+1))
          {
            tmp1<-rep(start+j-1,1000)
            if(j==1)
            {
              size1<-tmp1
              Delta1<-Delta[,j,i]
              power1 <-power[1:1000]
            }

            else
            {
              size1<-t(cbind(t(size1),t(tmp1)))
              Delta1<-t(cbind(t(Delta1),t(Delta[,j,i])))
              power1 <-t(cbind(t(power1),t(power[1:1000])))
            }
          }
          data<-data.frame(size1, Delta1, power1)
          l<-c(unique(size1))
          data$size1<- factor(data$size1, levels = l)

          gr<- ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=size1 , shape=size1,color=size1)) +
            ggplot2::geom_line(size=1.5)   +
            ggplot2::labs(title =ifelse(plot_factor=="ALL",paste0("Delta vs Power"),ifelse( delta_type ==1,paste0("SD(",plot_factor,")/SD(noise) vs Power"),paste0("Range(",plot_factor,")/SD(noise) vs Power")))) +
            ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
            ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
            ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
            ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")


          for ( j in 1:(n.choose -start+1) )
          {
            if(j==1)
            {
              RR= rep(temp_n[[j]],4)
              Ddelta<-  c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,ifelse(i==2 , 1, 0)),3),
                          round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,ifelse(i==2 , 1, 0)),3),
                          "1.0","1.5")
              Ppower<-  c("0.8","0.9",
                          round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                             ifelse(i==2, (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                          round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                             ifelse(i==2,(1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))
            }
            else
            {
              RR<-t(cbind(t(RR),t(rep(temp_n[[j]],4)) ))
              Ddelta<-t(cbind(t(Ddelta),t( c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,ifelse(i==2 , 1, 0)),3),
                                             round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,ifelse(i==2 , 1, 0)),3),
                                             "1.0","1.5")) ))

              Ppower<-t(cbind(t(Ppower),t(  c("0.8","0.9",
                                              round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                                 ifelse(i==2, (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                                              round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                                 ifelse(i==2,(1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))) ))
            }
          }
          values<-data.frame(R=RR, Delta=Ddelta, Power=Ppower,
                             stringsAsFactors = FALSE)

        }
        return( list(gr=gr, value=values ))
        }

        output$Delta_graph2  <- shiny::renderPlot({Delta.graph2(input$nf2, input$fr2, order=max(input$checkGroup2),delta_type=input$delta_type2,delta=ifelse(rep(input$delta_type2==1,3),c(input$de1_2,input$de2_2,input$de3_2),c(input$de11_2,input$de12_2,input$de13_2)), beta=1-input$b2, alpha=input$a2,plot_factor=input$plot_order2)$gr})
        output$values2  <- shiny::renderTable({Delta.graph2(input$nf2, input$fr2, order=max(input$checkGroup2),delta_type=input$delta_type2,delta=ifelse(rep(input$delta_type2==1,3),c(input$de1_2,input$de2_2,input$de3_2),c(input$de11_2,input$de12_2,input$de13_2)), beta=1-input$b2, alpha=input$a2,plot_factor=input$plot_order2)$value})


      output$Size_graph2<-shiny::renderPlot({
        plots.2levFr(factor=input$nf2, fraction=input$fr2,order=max(input$checkGroup2) ,delta_type=input$delta_type2, delta=ifelse(rep(input$delta_type2==1,3),c(input$de1_2,input$de2_2,input$de3_2),c(input$de11_2,input$de12_2,input$de13_2)), deltao=1 , alpha=input$a2, beta=1-input$b2,type=2)

        }
        )

      output$power_graph2<-shiny::renderPlot({
        plots.2levFr(factor=input$nf2, fraction=input$fr2,order=max(input$checkGroup2) , delta_type=input$delta_type2,delta=ifelse(rep(input$delta_type2==1,3),c(input$de1_2,input$de2_2,input$de3_2),c(input$de11_2,input$de12_2,input$de13_2)), deltao=as.numeric(input$plot_delta2) , alpha=input$a2, beta=1-input$b2,type=3)

        }
        )
      }
      }
      }
      )

    shiny::observeEvent(input$do3,
                 {
                   if ( input$nf3==1 &  max(input$checkGroup3)==2)
                   {
                     shinyalert::shinyalert("Error!", "The number of factor is just one! Two-way interactions can't be estimated.", type = "error")
                   }

                   else{
      list_tmp<-sizelist(input$nf3,max(input$checkGroup3))
      full_list<-list_tmp$full_list
      full_list1$RCBD<-c(full_list,"ALL")
      shiny::updateSelectInput(session,"plot_order3",choices=full_list1$RCBD)

      C<-Size.Block(input$nf3, as.numeric(unlist(strsplit(input$fl3,","))),  delta_type=input$delta_type3,order=max(input$checkGroup3),delta =ifelse(rep(input$delta_type3==1,3),c(input$de1_3,input$de2_3,input$de3_3),c(input$de11_3,input$de12_3,input$de13_3)), beta=1-input$b3, alpha=input$a3)
      if(length(C)==1)
      {
        shinyalert::shinyalert("Error!", "Sample size exceeds 1,000.", type = "error")
      }
      else {
      output$Size1_3<-shiny::renderText({C$n})
      output$Size2_3<-shiny::renderText({C$Delta})
      output$list1_3<-shiny::renderText({C$model})


      #####Graph
      Delta.graph3<-function (factor, factor.lev,delta_type, order,  delta =c(1,1,1), alpha=0.05, beta=0.2,plot_factor)
      {
        (n.choose <- C$n);
        (Delta.choose <- data.frame(t(C$Delta)))
        power <- round(seq(0,1,length.out=1001),3)
        start <- ifelse((n.choose-1)<=2, 2, n.choose-2)
        Delta <- array(0,c(1000,n.choose-start+1, ncol(Delta.choose)))
        delta.pwr <- matrix(0,n.choose-start+1, ncol(Delta.choose))

        temp_v<-list()
        temp_c<-list()
        temp_denom<-list()
        temp_n<-list()

        k<-1
        if( order==2)
        {
          v_flag<-c(rep(0,factor), rep(1,factor*(factor-1)/2))

        }
        else if (order==1){
          v_flag<-rep(0,factor+factor*(factor-1)/2)
        }

        for (n in start:(n.choose)) {
          v1=n-1
          if (order==1){
            v <- factor.lev-1
            c <- prod(factor.lev)*n/factor.lev
          }
          else if (order==2) {
            v <- (factor.lev-1)%*%t(factor.lev-1)
            v <- c(factor.lev-1,v[upper.tri(v, diag=FALSE)])
            c <- prod(factor.lev)*n/c(factor.lev, (factor.lev%*%t(factor.lev))[upper.tri((factor.lev)%*%t(factor.lev), diag=FALSE)])
          }
          v.denom <-  prod(factor.lev)*n-1-sum(v) -v1

          for (j in 1: length(v) ){

            for (ind in 1: 1000){
                        if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
                (Delta[ind,(n-start+1),j] <- fsize(alpha, 1-power[ind], v[j], v.denom, c[j], delta_type ,v_flag[j]))
              else (Delta[ind,(n-start+1),j]<-NA)
            }
          }
          temp_n[[k]]<-n
          temp_v[[k]]<-v
          temp_c[[k]]<-c
          temp_denom[[k]]<-v.denom
          k<-k+1
        }

        x<-seq(1:length(full_list1$RCBD))
        i<-x[full_list1$RCBD==plot_factor]

        if(i==max(x))
        {
          for(j in 1: length(v))
          {
            tmp1<-rep(full_list1$RCBD[[j]],1000)
            if ( j==1)
            {
              factor_type<-tmp1
              Delta1<-Delta[,(n.choose -start+1),j]
              power1 <-power[1:1000]
            }
            else if (j>1)
            {
              factor_type<-t(cbind(t(factor_type),t(tmp1)))
              Delta1<-t(cbind(t(Delta1),t(Delta[,(n.choose -start+1),j])))
              power1 <-t(cbind(t(power1),t(power[1:1000])))
            }

          }
          data<-data.frame(factor_type, Delta1, power1)
          l<-c(unique(factor_type))
          data$factor_type<- factor(data$factor_type, levels = l)

          gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
            ggplot2::geom_line(size=1.5)   +
            ggplot2::labs(title = "Delta vs Power") +
            ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
            ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
            ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
            ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

          values<-NULL
        }

        else if(i<max(x))
        {


          for( j in 1: (n.choose -start+1))
          {
            tmp1<-rep(start+j-1,1000)
            if(j==1)
            {
              size1<-tmp1
              Delta1<-Delta[,j,i]
              power1 <-power[1:1000]
            }

            else
            {
              size1<-t(cbind(t(size1),t(tmp1)))
              Delta1<-t(cbind(t(Delta1),t(Delta[,j,i])))
              power1 <-t(cbind(t(power1),t(power[1:1000])))
            }
          }

          data<-data.frame(size1, Delta1, power1)
          l<-c(unique(size1))
          data$size1<- factor(data$size1, levels = l)

          gr<- ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=size1 , shape=size1,color=size1)) +
            ggplot2::geom_line(size=1.5)   +
            ggplot2::labs(title =ifelse(plot_factor=="ALL",paste0("Delta vs Power"),ifelse( delta_type ==1,paste0("SD(",plot_factor,")/SD(noise) vs Power"),paste0("Range(",plot_factor,")/SD(noise) vs Power")))) +
            ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
            ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
            ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
            ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

          for ( j in 1:(n.choose -start+1) )
          {
            if(j==1)
            {
              RR= rep(temp_n[[j]],4)
              Ddelta<-  c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                          round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                          "1.0","1.5")
              Ppower<- c("0.8","0.9",
                         round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                            ifelse( v_flag[i] ==1 ,
                                                                                                                                    (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                         round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                            ifelse( v_flag[i] ==1,
                                                                                                                                    (1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))
            }
            else
            {
              RR<-t(cbind(t(RR),t(rep(temp_n[[j]],4)) ))
              Ddelta<-t(cbind(t(Ddelta),t( c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                                             round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]],temp_c[[j]][i],delta_type,v_flag[i]),3),
                                             "1.0","1.5")) ))

              Ppower<-t(cbind(t(Ppower),t( c("0.8","0.9",
                                             round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                                ifelse( v_flag[i] ==1 ,
                                                                                                                                                        (1*temp_c[[j]][i]),(1*temp_c[[j]][i]/2))))),3),#(Delta^2)*(c*nu1),
                                             round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]]),temp_v[[j]][i],temp_denom[[j]],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i])),
                                                                                                                                                ifelse( v_flag[i] ==1,
                                                                                                                                                        (1.5^2*temp_c[[j]][i]),(1.5^2*temp_c[[j]][i]/2))))),3))) ))
            }
          }
          values<-data.frame(R=RR, Delta=Ddelta, Power=Ppower,
                             stringsAsFactors = FALSE)

        }
        return( list(gr=gr, value=values ))
      }
      output$Delta_graph3  <- shiny::renderPlot({Delta.graph3(input$nf3, as.numeric(unlist(strsplit(input$fl3,","))),  order=max(input$checkGroup3), delta_type=input$delta_type3,delta =ifelse(rep(input$delta_type3==1,3),c(input$de1_3,input$de2_3,input$de3_3),c(input$de11_3,input$de12_3,input$de13_3)), beta=1-input$b3, alpha=input$a3,plot_factor=input$plot_order3 )$gr})
      output$values3  <- shiny::renderTable({Delta.graph3(input$nf3, as.numeric(unlist(strsplit(input$fl3,","))),  order=max(input$checkGroup3), delta_type=input$delta_type3,delta =ifelse(rep(input$delta_type3==1,3),c(input$de1_3,input$de2_3,input$de3_3),c(input$de11_3,input$de12_3,input$de13_3)), beta=1-input$b3, alpha=input$a3,plot_factor=input$plot_order3 )$value})

      output$Size_graph3<-shiny::renderPlot({
        plots.Block(input$nf3, as.numeric(unlist(strsplit(input$fl3,","))),  order=max(input$checkGroup3), delta_type=input$delta_type3,delta =ifelse(rep(input$delta_type3==1,3),c(input$de1_3,input$de2_3,input$de3_3),c(input$de11_3,input$de12_3,input$de13_3)),deltao=1, alpha=input$a3, beta=1-input$b3, type=2)

        }
        )

      output$power_graph3<-shiny::renderPlot({
        plots.Block(input$nf3, as.numeric(unlist(strsplit(input$fl3,","))),  order=max(input$checkGroup3), delta_type=input$delta_type3,delta =ifelse(rep(input$delta_type3==1,3),c(input$de1_3,input$de2_3,input$de3_3),c(input$de11_3,input$de12_3,input$de13_3)), deltao=as.numeric(input$plot_delta3), alpha=input$a3, beta=1-input$b3, type=3)


      }
      )
      }
                   }
                 }
    )
    shiny::observeEvent(input$do4,
      {

        if (input$wf>2 || input$sf>2)
        {
          shinyalert::shinyalert("Error!", "The number of whole plot factors and split plot factors are up to 2.", type = "error")
        }
        else {
        list_tmp<-sizelist.split(input$wf,input$sf, max(input$checkGroup4))
        full_list<-list_tmp$full_list
        full_list1$split<-c(full_list,"ALL")
        shiny::updateSelectInput(session,"plot_order4",choices=full_list1$split)

        D<-Size.Split(input$wf, as.numeric(unlist(strsplit(input$wfl,","))), input$sf, as.numeric(unlist(strsplit(input$sfl,","))),
                      delta_type=input$delta_type4,order=max(input$checkGroup4), delta=ifelse(rep(input$delta_type4==1,4),c(input$de1_4,input$de2_4,input$de3_4,input$de4_4),c(input$de11_4,input$de12_4,input$de13_4,input$de14_4)), beta=1-input$b4, alpha=input$a4)
        if(length(D)==1)
        {
          shinyalert::shinyalert("Error!", "Sample size exceeds 1,000.", type = "error")
        }
        else {
        output$Size1_4<-shiny::renderText({D$n})
        output$Size2_4<-shiny::renderText({D$Delta})
        output$list1_4<-shiny::renderText({D$model})


        Delta.graph4<-function(whole.factor, whole.factor.lev, split.factor, split.factor.lev, delta_type ,order , delta ,deltao, alpha , beta ,type,plot_factor)
        {
          (n.choose <- D$n);
          (Delta.choose <- data.frame(t(D$Delta)))
          power <- round(seq(0,1,length.out=1001),3)
          start <- ifelse((n.choose-1)<=2, 2, n.choose-2)
          Delta <- array(0,c(1000,n.choose-start+1, ncol(Delta.choose)))

          temp_n<-list()
          temp_v<-list()
          temp_c<-list()
          temp_denom<-list()
          k<-1

          if( order==2)
          {
            wv_flag<-c(rep(0,whole.factor), rep(1,whole.factor*(whole.factor-1)/2))
            sv_flag<-c(rep(0,split.factor), rep(1,split.factor*(split.factor-1)/2), rep(1,whole.factor*split.factor))
            vv_flag<-c(wv_flag,sv_flag)
          }
          else if (order==1){
            wv_flag<-rep(0,whole.factor )
            sv_flag<-rep(0,split.factor)
            vv_flag<-c(wv_flag,sv_flag)
          }

          for (n in start:(n.choose)) {
            if (order==1){
              v.whole <- NULL ; v.split <- NULL ; v.split.temp <- NULL
              c.whole <- NULL ; c.split <- NULL
              whole.Delta <- NULL; split.Delta <- NULL
              v.rep <- n-1

              #whole
              v.whole <-  whole.factor.lev-1
              v.whole.denom <- prod( whole.factor.lev)*n-1-sum( whole.factor.lev-1)-v.rep
              c.whole <- prod( whole.factor.lev)*prod( split.factor.lev)*n/ whole.factor.lev

              for (j in 1: length(v.whole)){
                for (ind in 1: 1000){
                            if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
                    (Delta[ind,(n-start+1),j] <- fsize(alpha, 1-power[ind],v.whole[j], v.whole.denom, c.whole[j],delta_type,0)*sqrt(prod( split.factor.lev)+1))
                  else ( Delta[ind,(n-start+1),j]<-NA)
                }
              }

              #split
              v.split <-  split.factor.lev-1
              v.split.denom <- prod( whole.factor.lev)*prod( split.factor.lev)*n-1-(prod( whole.factor.lev)*n-1)-sum( split.factor.lev-1)
              c.split <- prod( whole.factor.lev)*prod( split.factor.lev)*n/ split.factor.lev

              for (j in 1: length(v.split)){
                for (ind in 1: 1000){
                            if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
                    (Delta[ind,(n -start+1),(length(v.whole)+j)] <- fsize(alpha, 1-power[ind], v.split[j], v.split.denom, c.split[j],delta_type,0))
                  else ( Delta[ind,(n -start+1),(length(v.whole)+j)] <-NA)
                }
              }
            }
            else if (order==2){
              v.whole <- NULL ; v.split <- NULL ; v.split.temp <- NULL
              c.whole <- NULL ; c.split <- NULL
              whole.Delta <- NULL; split.Delta <- NULL
              v.rep <- n-1

              #whole
              v.whole <- ( whole.factor.lev-1)%*%t( whole.factor.lev-1)
              v.whole <- c( whole.factor.lev-1,v.whole[upper.tri(v.whole, diag=FALSE)])
              v.whole.denom <- prod( whole.factor.lev)*n-1-sum(v.whole)-v.rep
              c.whole <- prod( whole.factor.lev)*prod( split.factor.lev)*n/c( whole.factor.lev, ( whole.factor.lev%*%t( whole.factor.lev))[upper.tri(( whole.factor.lev)%*%t( whole.factor.lev), diag=FALSE)])

              for (j in 1: length(v.whole)){
                for (ind in 1: 1000){
                            if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
                    (Delta[ind,(n -start+1),j] <- fsize(alpha, 1-power[ind],v.whole[j], v.whole.denom, c.whole[j],delta_type,wv_flag[j])*sqrt(prod( split.factor.lev)+1))
                  else (Delta[ind,(n -start+1),j] <-NA)
                }
              }

              #split
              v.split <- ( split.factor.lev-1)%*%t( split.factor.lev-1)
              v.split.temp <- (whole.factor.lev-1)%*%t( split.factor.lev-1)
              v.split <- c( split.factor.lev-1, v.split[upper.tri(v.split, diag=FALSE)], as.vector(t(v.split.temp)))
              v.split.denom <- prod( whole.factor.lev)*prod(split.factor.lev)*n-prod( whole.factor.lev)*n-sum(v.split)
              c.split <- prod( whole.factor.lev)*prod( split.factor.lev)*n/c( split.factor.lev, ( split.factor.lev%*%t( split.factor.lev))[upper.tri(( split.factor.lev)%*%t(split.factor.lev), diag=FALSE)], as.vector(t( whole.factor.lev%*%t( split.factor.lev))))

              for (j in 1: length(v.split)){
                for (ind in 1: 1000){
                            if (alpha + 1 - power[ind] < 0.9999 & 1 - power[ind] > 0.0001 & 1 - power[ind] <0.9999 )
                    (Delta[ind,(n -start+1),(length(v.whole)+j)] <- fsize(alpha, 1-power[ind], v.split[j], v.split.denom, c.split[j],delta_type, sv_flag[j]))
                  else (Delta[ind,(n -start+1),(length(v.whole)+j)] <-NA)
                }
              }
            }
            temp_n[[k]]<-n
            temp_v[[k]]<-c(v.whole,v.split)
            temp_c[[k]]<-c(c.whole,c.split)
            temp_denom[[k]]<-c(v.whole.denom,v.split.denom)
            k<-k+1
          }


          x<-seq(1:length(full_list1$split))
          i<-x[full_list1$split==plot_factor ]


          if(i==max(x))
          {
            for(j in 1: (length(v.whole)+length(v.split)))
            {
              tmp1<-rep(full_list1$split[[j]],1000)
              if ( j==1)
              {
                factor_type<-tmp1
                Delta1<-Delta[,(n.choose -start+1),j]
                power1 <-power[1:1000]
              }
              else if (j>1)
              {
                factor_type<-t(cbind(t(factor_type),t(tmp1)))
                Delta1<-t(cbind(t(Delta1),t(Delta[,(n.choose -start+1),j])))
                power1 <-t(cbind(t(power1),t(power[1:1000])))
              }

            }
            data<-data.frame(factor_type, Delta1, power1)
            l<-c(unique(factor_type))
            data$factor_type<- factor(data$factor_type, levels = l)

            gr<-ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=factor_type , shape=factor_type,color=factor_type)) +
              ggplot2::geom_line(size=1.5)   +
              ggplot2::labs(title = "Delta vs Power") +
              ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
              ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
              ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
              ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

            values<-NULL
          }

          else if(i<max(x))
          {


            for( j in 1: (n.choose -start+1))
            {
              tmp1<-rep(start+j-1,1000)
              if(j==1)
              {
                size1<-tmp1
                Delta1<-Delta[,j,i]
                power1 <-power[1:1000]
              }

              else
              {
                size1<-t(cbind(t(size1),t(tmp1)))
                Delta1<-t(cbind(t(Delta1),t(Delta[,j,i])))
                power1 <-t(cbind(t(power1),t(power[1:1000])))
              }
            }

            data<-data.frame(size1, Delta1, power1)
            l<-c(unique(size1))
            data$size1<- factor(data$size1, levels = l)

            gr<- ggplot2::ggplot(data ,ggplot2::aes(x=Delta1, y=power1, group=size1 , shape=size1,color=size1)) +
              ggplot2::geom_line(size=1.5)   +
              ggplot2::labs(title =ifelse(plot_factor=="ALL",paste0("Delta vs Power"),ifelse( delta_type ==1,paste0("SD(",plot_factor,")/SD(noise) vs Power"),paste0("Range(",plot_factor,")/SD(noise) vs Power")))) +
              ggplot2::ylab("Power") +ggplot2::theme(axis.title.y=ggplot2::element_text(angle=90,   size=12))+
              ggplot2::xlab("Delta") +ggplot2::theme(axis.title.y=ggplot2::element_text(    size=12))+
              ggplot2::geom_hline(yintercept = c(0.8,  0.9),linetype = "dashed")+
              ggplot2::geom_vline(xintercept = c(1.0,1.5),linetype = "dashed")

            for ( j in 1:(n.choose -start+1) )
            {
              if(j==1)
              {
                RR= rep(temp_n[[j]],4)
                Ddelta<- c(round(fsize(alpha,0.2,temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)],temp_c[[1]][i],delta_type,vv_flag[i])*ifelse(i<=length(v.whole),sqrt(prod( split.factor.lev)+1),1),3),
                           round(fsize(alpha,0.1,temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)],temp_c[[1]][i],delta_type,vv_flag[i])*ifelse(i<=length(v.whole),sqrt(prod( split.factor.lev)+1),1),3),
                           "1.0","1.5")

                Ppower<-c("0.8","0.9",
                          round((1-stats::pf(stats::qf((1-alpha),temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)]),temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)],ncp=ifelse(delta_type==1,(1*(temp_c[[1]][i]*temp_v[[1]][i]))/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),
                                                                                                                                                                                             ifelse( vv_flag[i]==1, (1*temp_c[[1]][i])/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),(1*temp_c[[1]][i]/2)/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1))))),3),#(Delta^2)*(c*nu1),
                          round((1-stats::pf(stats::qf((1-alpha),temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)]),temp_v[[1]][i],temp_denom[[1]][ifelse(i<=length(v.whole),1,2)],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[1]][i]*temp_v[[1]][i]))/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),
                                                                                                                                                                                             ifelse( vv_flag[i]==1, (1.5^2*temp_c[[1]][i])/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),(1.5^2*temp_c[[1]][i]/2)/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1))))),3))

              }
              else
              {
                RR<-t(cbind(t(RR),t(rep(temp_n[[j]],4)) ))
                Ddelta<-t(cbind(t(Ddelta),t(  c(round(fsize(alpha,0.2,temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)],temp_c[[j]][i],delta_type,vv_flag[i])*ifelse(i<=length(v.whole),sqrt(prod( split.factor.lev)+1),1),3),
                                                round(fsize(alpha,0.1,temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)],temp_c[[j]][i],delta_type,vv_flag[i])*ifelse(i<=length(v.whole),sqrt(prod( split.factor.lev)+1),1),3),
                                                "1.0","1.5")) ))

                Ppower<-t(cbind(t(Ppower),t( c("0.8","0.9",
                                               round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)]),temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)],ncp=ifelse(delta_type==1,(1*(temp_c[[j]][i]*temp_v[[j]][i]))/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),
                                                                                                                                                                                                                  ifelse( vv_flag[i]==1, (1*temp_c[[j]][i])/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),(1*temp_c[[j]][i]/2)/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1))))),3),#(Delta^2)*(c*nu1),
                                               round((1-stats::pf(stats::qf((1-alpha),temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)]),temp_v[[j]][i],temp_denom[[j]][ifelse(i<=length(v.whole),1,2)],ncp=ifelse(delta_type==1,(1.5^2*(temp_c[[j]][i]*temp_v[[j]][i]))/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),
                                                                                                                                                                                                                  ifelse( vv_flag[i]==1, (1.5^2*temp_c[[j]][i])/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1),(1.5^2*temp_c[[j]][i]/2)/ifelse(i<=length(v.whole),prod(split.factor.lev)+1,1))))),3))) ))
              }
            }
            values<-data.frame(R=RR, Delta=Ddelta, Power=Ppower,
                               stringsAsFactors = FALSE)

          }
          return( list(gr=gr, value=values ))
        }

        output$Delta_graph4  <- shiny::renderPlot({Delta.graph4(input$wf, as.numeric(unlist(strsplit(input$wfl,","))), input$sf, as.numeric(unlist(strsplit(input$sfl,","))),
                                                         delta_type=input$delta_type4,order=max(input$checkGroup4),  delta =ifelse(rep(input$delta_type4==1,4),c(input$de1_4,input$de2_4,input$de3_4,input$de4_4),c(input$de11_4,input$de12_4,input$de13_4,input$de14_4)), beta=1-input$b4, alpha=input$a4,plot_factor=input$plot_order4)$gr})
        output$values4  <-  shiny::renderTable({Delta.graph4(input$wf, as.numeric(unlist(strsplit(input$wfl,","))), input$sf, as.numeric(unlist(strsplit(input$sfl,","))),
                                         delta_type=input$delta_type4,order=max(input$checkGroup4),  delta =ifelse(rep(input$delta_type4==1,4),c(input$de1_4,input$de2_4,input$de3_4,input$de4_4),c(input$de11_4,input$de12_4,input$de13_4,input$de14_4)), beta=1-input$b4, alpha=input$a4,plot_factor=input$plot_order4)$value})



       output$Size_graph4<- shiny::renderPlot({
          plots.Split(input$wf, as.numeric(unlist(strsplit(input$wfl,","))), input$sf, as.numeric(unlist(strsplit(input$sfl,","))),
                    delta_type=input$delta_type4,order=max(input$checkGroup4),  delta =ifelse(rep(input$delta_type4==1,4),c(input$de1_4,input$de2_4,input$de3_4,input$de4_4),c(input$de11_4,input$de12_4,input$de13_4,input$de14_4)), beta=1-input$b4, alpha=input$a4,type=2)

        }
        )

       output$power_graph4<- shiny::renderPlot({
         plots.Split(input$wf, as.numeric(unlist(strsplit(input$wfl,","))), input$sf, as.numeric(unlist(strsplit(input$sfl,","))),
                     delta_type=input$delta_type4,order=max(input$checkGroup4),  delta =ifelse(rep(input$delta_type4==1,4),c(input$de1_4,input$de2_4,input$de3_4,input$de4_4),c(input$de11_4,input$de12_4,input$de13_4,input$de14_4)), deltao=as.numeric(input$plot_delta4),beta=1-input$b4, alpha=input$a4,type=3)


        }
        )
##########################3
        }
        }
       }
    )

    }#end server
  )#end App

  shiny::runApp(Size_app,launch.browser=TRUE)
}
  }

