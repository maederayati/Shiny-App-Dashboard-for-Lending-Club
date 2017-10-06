library(shiny)
library(ggplot2)
library(Scale)
library(scales)
library(zoo)
library(corrplot)
library(knitr)
library(gridExtra)
library(corrplot)
library(grid)




function(input, output) {

        load("df.RData")
  
    filt<-function(a,imin,imax, imin2,imax2,icheck1,icheck2,
                   idatemin, idatemax){
        b<-a[(a[,1]>=imin &
                     a[,1]<=imax &
                     a[,2]>=imin2 &
                     a[,2]<=imax2 &
                     a[,4] %in% icheck1 &
                     a[,3]>=idatemin &
                     a[,3]<=idatemax &
                     a[,5] %in% icheck2),]
        b
    }
    
  render_related_plot<-reactive({
        #  df<-cbind.data.frame(d_int_rate,d_loan_amnt,d_issue_date, d_loan_term, d_loan_status, d_installment, d_total_pymnt,d_total_int)
        #  names(df)<-c("d_int_rate","d_loan_amnt","d_issue_date", "d_loan_term","d_loan_status","d_installment","d_total_pymnt","d_total_int")
        l<-list()  
        imin<-input$int_rate_range[1]
          imax<-input$int_rate_range[2]
          imin2<-input$loan_amnt_range[1]
          imax2<-input$loan_amnt_range[2]
          icheck1<-input$loan_term_check
          icheck2<-input$loan_status_check
          idatemin<-as.Date(input$issue_date_range[1])
          idatemax<-as.Date(input$issue_date_range[2])
          dfr<-filt(df,imin,imax, imin2,imax2,icheck1,icheck2,
               idatemin, idatemax)
          
          term<-as.vector(sapply(as.character(as.vector(dfr[,3])),function(x){
              if(x=="36 months")
                  36
              else 
                  60
          }))
          
          #calculate the numbers
          summary<-data.frame( 
              Description=c("Total Loan Amount", "Total to-be-recieved Payments","Total to-be-recieved Income","Total Recieved Payments"),
              Value=c(
                  paste0("$",prettyNum(sum(dfr[,2], na.rm=T),
                                       big.mark=",",scientific=FALSE)),
                  paste0("$",prettyNum(sum(term*as.vector(dfr[,6])),
                                       big.mark=",",scientific=FALSE)), 
                  paste0("$",prettyNum(sum(term*as.vector(dfr[,6]))-sum(as.vector(dfr[,2]),
                                                                       na.rm=T),big.mark=",",scientific=FALSE)),
                  paste0("$",prettyNum(sum(dfr[,7], na.rm=T),big.mark=",",scientific=FALSE))
              ))
          summary
          #rownames(summary)<-NULL
          l[[1]]<-summary
          
          #plot10<-tableGrob(summary, rows=NULL)
          #grid.draw(plot10)
      

  
  ###################
#  render_related_plot2<-reactive({
    
      
      options(scipen=10000)
      dft<-as.data.frame(dfr[,2])
      names(dft)<-"weight"
      plot1<-ggplot(dft, aes(x="",y=weight)) + 
          geom_boxplot(fill = "white", 
                       colour = "#3366FF")+
          ylab("")+xlab("Loan Amount")
      
    l[[2]]<-plot1
         
  
  
      
      #boxplot for interest rate
      dft<-as.data.frame(dfr[,7])
      names(dft)<-"weight"
      plot2<-ggplot(dft, aes(x="",y=weight)) + geom_boxplot(fill = "white", colour = "#3366FF")+
          ylab("")+xlab("Recieved Payments")
      plot2
      
  l[[3]]<-plot2
  
  
  
      #boxplot for Total recieved interest
      dft<-as.data.frame(dfr[,8])
      names(dft)<-"weight"
      plot3<-ggplot(dft, aes(x="",y=weight)) + geom_boxplot(fill = "white", colour = "#3366FF")+
          ylab("")+xlab("Recieved Interest")
      plot3
  l[[4]]<-plot3
  
  
  
      #boxplot for interest rate
      dft<-as.data.frame(dfr[,1])
      names(dft)<-"weight"
      plot4<-ggplot(dft, aes(x="",y=weight)) + geom_boxplot(fill = "white", colour = "#3366FF")+
          ylab("")+xlab("Interest Rate")
  l[[5]]<-plot4
  
      plot5<- ggplot(dfr)+geom_histogram(aes(x=dfr[,1]), bins=30, colour="black", fill="blue", alpha=0.5)+
          guides(fill=guide_legend(title="Loan Term"))+
          xlab("Interst Rate")+
          ylab("Number of Loans ")+ggtitle("Interest Rate vs. Number of Loans")
      plot5
  l[[6]]<-plot5
  
  plot6<- ggplot(dfr)+geom_histogram(aes(x=dfr[,2]), bins=30, colour="black", fill="blue", alpha=0.5)+
          guides(fill=guide_legend(title="Loan Term"))+
          xlab("Loan Amount")+
          ylab("Number of Loans ")+ggtitle("Loan Amount vs. Number of Loans")
      plot6
  l[[7]]<-plot6
  

      plot7<- ggplot(dfr)+geom_histogram(aes(x=dfr[,3]), bins=30, colour="black",fill="blue", alpha=0.5)+
          guides(fill=guide_legend(title="Loan Term"))+
          xlab("Issued Date")+
          ylab("Number of Loans ")+ggtitle("Issued Date vs. Number of Loans")
      plot7
      
  l[[8]]<-plot7
  

      tdf<-as.data.frame(table(dfr[,4]))
      tdf[,2]<-tdf[,2]/sum(tdf[,2])
      names(tdf)<-c("var","value")
      plot8<- ggplot(tdf, aes(x="", y=value, fill=var))+
          geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0) +
          guides(fill=guide_legend(title="Loan Term"))+xlab("")+ylab("")+ggtitle("Loan Term")
      plot8
  l[[9]]<-plot8

      tdf<-as.data.frame(table(dfr[,5]))
      names(tdf)<-c("var","value")
      tdf[,1]<-as.character(tdf[,1])
      tdf[,2]<-round((tdf[,2]/sum(tdf[,2]))*100,1)
      plot9<-ggplot(tdf, aes(x = var, y = value)) + 
          geom_bar(stat = "identity", colour="black", fill="blue", alpha=0.5)+
          geom_text(aes(label = value), size = 3,  vjust = -0.3)+
          theme(axis.text.x = element_text(angle = 20, hjust = 1))+
          xlab("")+ylab("% of Loans")+
          title("Loan Status") +
          theme(plot.title = element_text(hjust = 0.5)) 
      
     l[[10]]<-plot9
     
     
     
     
     #pie chart for employment length of borrower
     tdf<-as.data.frame(table(dfr[,9]))
     names(tdf)<-c("var","value")
     tdf[,2]<-tdf[,2]/sum(tdf[,2])
   
     plot11<- ggplot(tdf, aes(x="", y=value, fill=var))+
         geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0) +
         guides(fill=guide_legend(title="Borrowers' Employment Length"))+xlab("")+ylab("")+ggtitle("Employment Length Distribution")
     
     l[[11]]<-plot11
     
     #pie chart of home ownership status of customers
     tdf<-as.data.frame(table(dfr[,10]))
     names(tdf)<-c("var","value")
     tdf[,2]<-tdf[,2]/sum(tdf[,2])
     plot12<- ggplot(tdf, aes(x="", y=value, fill=var))+
         geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0) +
         guides(fill=guide_legend(title="Home Ownership"))+xlab("")+ylab("")+ggtitle("Home Ownership Distribution")
     
     
     l[[12]]<-plot12
     
     
     
     
     l
      
 
             })
  
  
  
  
  
    ###################
  output$show_plot1<-renderDataTable(render_related_plot()[[1]], options = list(autoWidth = F,
                                                                            columnDefs = list(list(
                                                                                width = '20px', target = 3
                                                                            ))))
  output$show_plot2<-renderPlot({plot.new();render_related_plot()[[2]]})
  output$show_plot3<-renderPlot(render_related_plot()[[3]])
  output$show_plot4<-renderPlot(render_related_plot()[[4]])
  output$show_plot5<-renderPlot(render_related_plot()[[5]])
  output$show_plot6<-renderPlot(render_related_plot()[[6]])
  output$show_plot7<-renderPlot(render_related_plot()[[7]])
  output$show_plot8<-renderPlot(render_related_plot()[[8]])
  output$show_plot9<-renderPlot(render_related_plot()[[9]])
  output$show_plot10<-renderPlot(render_related_plot()[[10]])
  output$show_plot11<-renderPlot(render_related_plot()[[11]])
  output$show_plot12<-renderPlot(render_related_plot()[[12]])
  
  
  
  
  render_related_cor<-reactive({
      
      load("loan_related_vars.RData")
            load("borrower_related_vars.RData")
            load("correlation_all.RData")
            #load("nqd.RData")
            
            # load("res1_80.RData")
            # if(input$confLevel==0.80){
            #     load("res1_80.RData")
            #     res1<-res1_80
            # }
            # if(input$confLevel==0.85){
            #     load("res1_85.RData")
            #     res1<-res1_85
            # }
            # if(input$confLevel==0.90){
            #     load("res1_90.RData")
            #     res1<-res1_90
            # }
            # if(input$confLevel==0.95){
            #     load("res1_95.RData")
            #     res1<-res1_95
            # }
            
            if(input$Loan_vs_borrower=="loan_borrower"){
                
                corplot1<-corrplot(correlation_all[borrower_related_vars,loan_related_vars],#p.mat = res1[[1]][borrower_related_vars,loan_related_vars],
                                   method="color",tl.cex = 1, tl.col = 'black',na.label = NA,
                                   title="Correlation Matrix for Loan-Related vs. Borrower-Related Attributes",
                                   mar=c(0,1,1,0)
                                   )
                corplot1
      
            }else if(input$Loan_vs_borrower=="loan_loan"){
      
                corplot2<-corrplot(correlation_all[loan_related_vars,loan_related_vars],#p.mat = res1[[1]][loan_related_vars,loan_related_vars],
                                   method="color",tl.cex = 1,
                                   tl.col = 'black',na.label = NA,
                                   title="Correlation Matrix for Loan-Related vs. Loan-Related Attributes" ,
                                   mar=c(0,0,1,0))
                corplot2
      
      
            }else{
      
                corplot3<-corrplot(correlation_all[borrower_related_vars,borrower_related_vars],#p.mat = res1[[1]][borrower_related_vars,borrower_related_vars],
                                   method="color",
                                   tl.cex = 1, 
                                   tl.col = 'black',na.label = NA, 
                                   title="Correlation Matrix for Borrower-Related vs. Borrower-Related Attributes", 
                                   mar=c(0,0,1,0))
                corplot3
      
      
            }
      

      
      
  })
  
  output$show_cor_plot<-renderPlot(render_related_cor())
 
  select_text<-reactive({
      if(input$Loan_vs_borrower=="loan_loan")
          HTML(" <h4>The Graph bellow shows the following:</h4>
    <ul>  
   <li>The higher is he loan amount the longer is the loan term.</li>

    <li>           The longer is the loan term the higher is the interest rate.</li>
               
               
    <li>           The loans which have the charged off or late status tend to have higher interst rates. Also the charged off or late loan pay more recoveries.</li>
               
    </ul>           
               ")
      
      else if(input$Loan_vs_borrower=="borrower_borrower")
          HTML(" <h4>The Graph bellow shows the following:</h4>
               <ul>  
            <li>  The people having higher income tend to have higher joint income. Also the verfied customers tend to have verified joints.</li>

            <li>   The people having higher revolving balance have higher revolving high credit divided by credit limit (total_rev_hi_lim).</li>
               
             <li>  The people with higher number of installment accounts opened in past 24 months tend to have lower grade (riskier grade).</li>
               
              <li> People with lower grade (riskier) have higher number of inquiries in the last 12 month.</li>
               
             <li>  People with higher revolving high credit divided by credit limit tend to open more open accounts.</li>
               
              <li> The higher is the annual income of a person the higher is the current total balance. </li>
               </ul>           
               ")
      else
          HTML(" <h4>The Graph bellow shows the following:</h4>
               <ul>  
               <li> The higher is the income of a borrower the higher is the loan amount.</li>
               
               <li>   The higher is the income of a borrower's joint the higher is the loan amount.</li>
               
               <li>   The higher is the revolving balance of a borrower the higher is the loan amount.</li>
               
               <li>  The better is the grade of a borrower the lower is the interst rate, loan amount, loan term, total recieved interest and recoveries.
               
               <li>  The earlier is the issue date the earlier is the last credit pull date.
               
               <li>  The higher is the number of inquires in the last 12 months the higer is the interst rate.
               
               Verified borrowers get higher loan amounts.
               
               </ul>           
               ")
      
      
      
  })
  
  
  
  output$show_text1<-renderUI(select_text())
    
     
}