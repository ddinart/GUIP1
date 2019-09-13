.CRMB=function(pos=1, e)
{
  e$CRM.2 = NULL
  e$ResSim = NULL
  tclRequire("Tktable")
  remplireRequid=function()
  {
    .Sauvgarde.Study=function(...)
    {
      Nb_Dl=as.numeric(tclvalue(SliderValue))
      DL=NULL
      if (Nb_Dl==2) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))}
      if (Nb_Dl==3) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))}
      if (Nb_Dl==4) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))}
      if (Nb_Dl==5) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))}
      if (Nb_Dl==6) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))}
      if (Nb_Dl==7) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))}
      if (Nb_Dl==8) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))}

      Target=as.numeric(tclvalue(tkget((Target))))
      Model=as.numeric(tclvalue((rbValue.M)))

      if (Model==1){Model.n="logistic"; intcpt=as.numeric(tclvalue(tkget(intcpt.)))}
      if (Model==2){Model.n="empiric"; intcpt=NULL}

      Stop=as.numeric(tclvalue(rbValue.SC))
      N_Pat=as.numeric(tclvalue(tkget(NBpat)))
      N_pat_Level=as.numeric(tclvalue(tkget(NBpat)))

      if ((Stop == 1) & (N_Pat <=0)) stop('Number of patients to be enrolled in the study <=0')
      if ((Stop == 1) & (N_Pat > 100)) cat("\n Warning: Number of patients to be enrolled in the study > 100")
      if ((Stop == 2) & (N_Pat <=0)) stop('Number of patients by dose level  <=0')
      if ((Stop == 2) & (N_Pat > 20)) cat("\n Warning: Large number of patients by dose level")

      assign("names_Study", tclvalue(tkget(names.S)), envir = as.environment(pos))
      assign("Study",list(name=paste(names_Study) ,Nb_Dl=Nb_Dl, DL=DL, Target=Target, Model=Model, Model.n=Model.n,
                          Stop=Stop, N_Pat=N_Pat ,N_pat_Level=N_pat_Level, halfwidthPrior=halfwidthPrior,
                          nuPrior=nuPrior, Intcpt=intcptPrior, Sd=sdPrior), envir = as.environment(pos))

      differ=abs(Study$DL-Study$Target)
      MTD=1
      for ( i in 1 : Study$Nb_Dl)
      {
        if (differ[i]==min(differ))
        {MTD=i}
      }
      Pat_include=0
      PT.data=NULL
      assign("Res",list(MTD=MTD,Pat.included=Pat_include,PT.data=PT.data), envir = as.environment(pos))
      save(Res,Study,file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
      .ReCap()
      Study.file=function()
      {
        assign("a00", paste(names_Study), envir = as.environment(pos))
        a0=paste('Name of study',paste(names_Study),sep=':')
        a1=paste('Method','CRMB',sep=':')
        a2='Continual Reassessment Method (CRM) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(names_Study),'crmb', sep='.'))
      }
      Study.file()
      tk2notetab.select(net.CRMB , "Include")
    }
    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)
    tk2notetab.select(net.CRMB , "Input parameters")

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save, row=27, column=0, sticky="se")
    Nb_Niv_Doses<- nlevel
    priorProbaf <- c()
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (tkget(eval(parse(text = paste('valp',i,sep='')))))
      priorProbaf <- c(priorProbaf,as.numeric(tclvalue(n.iter)))
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter, state="disabled"),envir = as.environment(pos)),sticky="w",row=r,column=1)
    }

    if (any(priorProbaf <= 0) | any(priorProbaf >= 1)) stop('Prior probabilities must be within ]0,1[ ')
    if (is.unsorted(priorProbaf, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')

    SliderValue=tclVar(nlevel)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue,state="disabled",
                            resolution=1, orient="horiz")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(TargetPrior)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi) ; rb2 <- tkradiobutton(Saisi)
    rbValue.SC <- tclVar(1)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)
    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NBpat<-tkentry(Saisi, width=10)
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi) ; rb2.1 <- tkradiobutton(Saisi)
    rbValue.M <- tclVar(modelPrior)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic", state="disabled", font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric", state="disabled", font=PoliceGenerale)
    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Tsd.<-tklabel(Saisi, text="Normal s.deviation",font=PoliceGenerale)
    n.iter <- tclVar(sdPrior)
    sd.<-tkentry(Saisi,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=8,column=0, sticky="w")
    tkgrid(sd., row=8,column=1, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(intcptPrior)
    }else {tclVar("NA")}
    assign("intcpt.",tkentry(Saisi, textvariable=n.iter, width=10, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt., row=8,column=2, sticky="w")
    tkgrid(intcpt., row=8,column=3, sticky="w")

    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    Nam<- tclVar()
    names.S<-tkentry(Saisi, width=20)
    tkgrid(labage, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,command=.Sauvgarde.Study,bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    assign("etatPrior",TRUE,envir = as.environment(pos))
  }



  disabled.PriorCalib=function(...)
  {

    if(is.tkwin(affCAL)){tkdestroy(affCAL)}
    if(is.tkwin(SaisiCal)){tkdestroy(SaisiCal)}
    if(is.tkwin(Saisi.saveCal )){tkdestroy(Saisi.saveCal )}

    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)
    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)
    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))

    SliderValue.CAL=tclVar(Study$Nb_Dl)
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    for ( i in 1:Study$Nb_Dl)
    {
      r=10 + i
      assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
      n.iter=tclVar (tkget(eval(parse(text = paste('val',i,sep='')))))
      tkgrid(assign(paste('lab',i,sep=''),tklabel(affCAL,text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(affCAL,state="disabled", width=8,textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=1)
    }
    if(Study$Nb_Dl!=8)
    {
      for ( i in (Study$Nb_Dl+1):8)
      {
        r=10 + i
        tkgrid(assign(paste('lab',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
        tkgrid(assign(paste('val',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
      }
    }
    Titre=tklabel(SaisiCal,text="Target DLT rate ",font=PoliceGenerale)
    n.iter=tclVar(round(TargetPrior,3))
    Target.cal<-tkentry(SaisiCal,textvariable=n.iter,state="disabled", width=5)
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    n.iter=tclVar(halfwidthPrior)
    halfwidth<-tkentry(SaisiCal, width=10,textvariable=n.iter,state="disabled")
    tkgrid(lab_halfwidth, row=3,column=0, sticky="w", columnspan=2)
    tkgrid(halfwidth, row=3,column=2, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal,state="disabled") ; rb2.1CAL <- tkradiobutton(SaisiCal,state="disabled")
    assign("rbValue.MCAL", tclVar(modelPrior), envir = as.environment(pos))
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    n.iter=tclVar(nuPrior)
    nu<-tkentry(SaisiCal, width=10,textvariable=n.iter,state="disabled")
    tkgrid(lab_nu, row=6,column=0, sticky="w")
    tkgrid(nu, row=6,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="Normal s.deviation",font=PoliceGenerale)
    n.iter=tclVar(Study$Sd)
    sd.<-tkentry(SaisiCal,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=7,column=0, sticky="w")
    tkgrid(sd., row=7,column=1, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(Study$Intcpt)}
    else {tclVar("NA")}
    assign("intcpt.", tkentry(SaisiCal, textvariable=n.iter, width=10, state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,bg="cornflower blue",state="disabled")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,bg="cornflower blue",state="disabled")
    tkgrid(Validation, row=2)
  }

  PriorCalib=function(...)
  {

    .Crea.f.CAL=function(...)
    {
      assign("TargetPrior", as.numeric(tclvalue(tkget((Target.cal)))), envir = as.environment(pos))
      assign("halfwidthPrior", as.numeric(tclvalue(tkget((halfwidth)))), envir = as.environment(pos))
      assign("nuPrior", as.numeric(tclvalue(tkget(nu))), envir = as.environment(pos))
      assign("nlevel", as.numeric(tclvalue(tkget((slider_Nb_DL.CAL)))), envir = as.environment(pos))
      assign("modelPrior", as.numeric(tclvalue(((rbValue.MCAL)))), envir = as.environment(pos))
      assign("sdPrior", as.numeric(tclvalue(tkget((sd.)))), envir = as.environment(pos))
      assign("intcptPrior", as.numeric(tclvalue(tkget((intcpt.)))), envir = as.environment(pos))
      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))
      assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)
      if (TargetPrior>0.55) cat("\n Warning: Target DLT rate too high")
      if (TargetPrior<0.15) cat("\n Warning: Target DLT rate too low")
      if ((halfwidthPrior<=0)|(halfwidthPrior>0.5)) stop('Halfwidth of the indifference intervals incorrect!')
      if ((halfwidthPrior<=0.5)&(halfwidthPrior>0.2)) cat("\n Warning: Try to reduce the value of the halfwidth of the indifference intervals ")
      if ((nuPrior<1)|(nuPrior>Nb_Niv_DosesCAL)|(nuPrior%%1 != 0)) stop(paste('Prior guess of MTD incorrect, enter an integer between 1 and',Nb_Niv_DosesCAL))

      if (modelPrior==1)
      {
        e$prior<-dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior, nlevel, model="logistic", intcpt=intcptPrior)
      }

      if (modelPrior==2)
      {
        e$prior<-dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior, nlevel, model="empiric")
      }


      for ( i in 1: Nb_Niv_DosesCAL)
      {
        r=10 + i
        assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
        n.iter=tclVar(round(e$prior[i],3))
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,textvariable=n.iter),envir = as.environment(pos)),row=r,column=1)
      }

      if( Nb_Niv_DosesCAL!=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i
          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }
    }

    f.CALPrior=function(...)
    {
      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("affCAL",tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))

      for ( i in 1: Nb_Niv_DosesCAL)
      {
        r=10 + i
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8), envir = as.environment(pos)),row=r,column=1)
      }


      if(Nb_Niv_DosesCAL !=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i

          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }

    }

    .Param.choice = function(...)
    {
      if (as.numeric(tclvalue((rbValue.MCAL)))==2)
      {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt.", tkentry(SaisiCal, width=10, state="disabled"), envir = as.environment(pos))
        tkgrid(Tintcpt., row=5,column=2, sticky="w")
        tkgrid(intcpt., row=5,column=3, sticky="w")
      }
      else {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale)
        assign("intcpt.", tkentry(SaisiCal, width=10), envir = as.environment(pos))
        tkgrid(Tintcpt., row=5,column=2, sticky="w")
        tkgrid(intcpt., row=5,column=3, sticky="w")
      }
    }
    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)
    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)
    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))

    SliderValue.CAL=tclVar('8')
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz",command=f.CALPrior)
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    Titre=tklabel(SaisiCal,text="Target DLT rate ",font=PoliceGenerale)
    Target.cal<-tkentry(SaisiCal, width=5)
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    halfwidth<-tkentry(SaisiCal, width=10)
    tkgrid(lab_halfwidth, row=3,column=0, sticky="w", columnspan=2)
    tkgrid(halfwidth, row=3,column=2, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice)
    rb2.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice)
    rbValue.MCAL <- tclVar(1)
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="Normal s.deviation",font=PoliceGenerale)
    sd.<-tkentry(SaisiCal, width=10)
    tkgrid(Tsd., row=4,column=2, sticky="w")
    tkgrid(sd., row=4,column=3, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    assign("intcpt.", tkentry(SaisiCal, width=10), envir = as.environment(pos))
    tkgrid(Tintcpt., row=5,column=2, sticky="w")
    tkgrid(intcpt., row=5,column=3, sticky="w")

    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    nu<-tkentry(SaisiCal, width=10)
    tkgrid(lab_nu, row=7,column=0, sticky="w")
    tkgrid(nu, row=7,column=1, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,command=.Crea.f.CAL,bg="cornflower blue")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,command=remplireRequid,bg="cornflower blue")
    tkgrid(Validation, row=2)

  }

  loads <- function()
  {
    loads.infos <- tkmessageBox(title = "Data loading Infos",
                                message = "Select a file.crmb or file.cbsim", icon = "info", type = "ok")

    filef <- tclvalue(tkgetOpenFile())
    stu=read.table(filef,header=TRUE)
    assign("names_Study", paste(stu[1,]), envir = as.environment(pos))
    extension<-str_sub(filef,-4)

    if (extension == "crmb") {
      load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      e$CRM.2 <- CRM.2
      Open.CRM()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          tkdestroy(Include)
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            tkdestroy(Include)
          }
        }
      }
    }
    if (extension == "bsim") {
      load(file =paste(paste(names_Study,'cbsim',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      e$ResSim <- ResSim
      assign("StudyS",StudyS, envir = as.environment(pos))
      tkdestroy(Win_CRMB)
      frameInitSIM()
      disabled.SimCRM()

    }
  }

  pending_listing=function()
  {

    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
    dataa=Res$PT.data[which(Res$PT.data$pndg==1),]
    if (nrow(dataa)==0 )
    {
      msg <- paste("No patient pending",sep="")
      tkmessageBox(message=msg)
    }
    if (nrow(dataa)>=1)
    {
      assign("tt.pend", tktoplevel(), envir = as.environment(pos))
      tkwm.geometry(tt.pend, "+650+400")
      assign("tl",tklistbox(tt.pend,height=4,selectmode="single",background="white"), envir = as.environment(pos))
      tkgrid(tklabel(tt.pend,text="Select a patient",font=PoliceGenerale))
      tkgrid(tl)
      numPatP=paste("Patient N'", as.character(dataa$patid),sep="")
      for (i in (1:length(numPatP)))
      {
        tkinsert(tl,"end",numPatP[i])
      }
      tkselection.set(tl,0)
      OK.but <-tkbutton(tt.pend,text="  OK  ",command=.include.pending,bg="cornflower blue")
      tkgrid(OK.but)
      tkfocus(tt.pend)
    }
  }

  .include.pending <- function()
  {
    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
    data.pinding=Res$PT.data[which(Res$PT.data$pndg==1),]
    data.inclus=Res$PT.data[which(Res$PT.data$pndg==0),]
    numPatP=as.character(data.pinding$patid)
    assign("Choice", as.numeric(numPatP[as.numeric(tkcurselection(tl))+1]), envir = as.environment(pos))
    print(Choice)

    Inc=tktoplevel()
    tkwm.geometry(Inc, "+650+400")

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Inc.mm1)
    assign("Titre", tklabel(Inc.mm1,text="DLT",font=PoliceGenerale), envir = as.environment(pos))
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i))))
    }

    .Sauv2=function(...)
    {
      assign("DLT", as.numeric(tclvalue((rbValue.DLT))), envir = as.environment(pos))
      Newpat=data.pinding[which(data.pinding$patid==Choice),]

      Newpat$DLT=DLT
      Newpat$pndg=0
      DATA=rbind(data.inclus,Newpat)
      e$CRM.2 <- dfcrm::crm( prior=Study$DL, target=Study$Target, tox=DATA[,2],level=DATA[,1],method = "bayes", conf.level = 0.95,
                             model = paste(Study$Model.n), scale=Study$Sd, intcpt=Study$Intcpt)

      Res$MTD=e$CRM.2$mtd
      Res$PT.data[which(Res$PT.data$patid==Choice),]$pndg=0
      Res$PT.data[which(Res$PT.data$patid==Choice),]$DLT=DLT
      CRM.2 <- e$CRM.2
      save(Res,Study,CRM.2,file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
      load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
      tkdestroy(Inc)
      assign("Res",Res, envir = as.environment(pos))
      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }

      if(Res$Pat.included==1)
      {
        disabled.CRM()
        if (etatPrior==TRUE)
        {disabled.PriorCalib()}
        if (etatPrior==FALSE)
        {tkdestroy(PriorCal)}
      }


    }

    assign("Inc.Win_CRMB", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Inc.Win_CRMB,text="Validate",width=30,command=.Sauv2,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_CRMB)
    tkdestroy (tt.pend)

  }

  .ReCap=function(...)
  {
    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))
    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))
    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))
    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Continued reassessment method-bayesian approach (CRMB) "))
    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model==1,"Logistic","Empiric")))
    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))
    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_CRMB", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Rec.Win_CRMB,text="New patient",width=30,command=.include,bg="cornflower blue")
    Save.pend=tkbutton(Rec.Win_CRMB,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

    tkgrid(Rec.Win_CRMB, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    tkgrid(Save.pend, row=0, column=1)

  }

  disabled.ReCap=function(...)
  {
    tkdestroy(Rec.mm0)
    tkdestroy(Rec.Win_CRMB)

    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Continued reassessment method-bayesian approach (CRMB) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model==1,"Logistic","Empiric")))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"),font=PoliceGenerale)
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included),font=PoliceGenerale)

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"),font=PoliceGenerale)
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD),font=PoliceGenerale)

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    DATA=Res$PT.data[which(Res$PT.data$pndg==1),]
    if (nrow(DATA)==0)
    {
      print('CAS 1')
      assign("Rec.Win_CRMB", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_CRMB,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_CRMB,text="Pending patient",width=30,state="disabled",bg="cornflower blue")
      tk2notetab.select(net.CRMB ,"Results")
      print(' Fin CAS 1')

      tkgrid(Rec.Win_CRMB, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
    }
    if (nrow(DATA)!=0)
    {
      print('CAS 2')
      assign("Rec.Win_CRMB", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_CRMB,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_CRMB,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

      tkgrid(Rec.Win_CRMB, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
      print(' Fin CAS 2')
    }
  }

  .ReCap.Resultat=function(...)
  {

    Resultsbypatient=function()
    {
      assign("myRarray1", data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2]),
             envir = as.environment(pos))
      X=colnames(myRarray1)
      myRarray=rbind(X,myRarray1)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="25",background="white")
      tkgrid(t.d, row=2, column=0)

    }

    ResultsbyLevel=function()
    {
      Prior=round(e$CRM.2$prior,3)
      n.patient=NULL

      total.tox=NULL
      ptox=round(e$CRM.2$ptox,3)
      ptoxL=round(e$CRM.2$ptoxL,3)
      ptoxU=round(e$CRM.2$ptoxU,3)
      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }

      for (i in 1 : Study$Nb_Dl)
      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}
        total.tox=c(total.tox,res2)
      }

      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, n.patient=n.patient,
                                     total.tox=total.tox, ptox=ptox, ptoxL=ptoxL, ptoxU=ptoxU), envir = as.environment(pos))
      X=colnames(myRarray2)
      myRarray=rbind(X,myRarray2)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="15",background="white")
      tkgrid(t.d, row=2, column=0)
    }

    ExportRes=function()
    {

      myRarray1 <- data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2])
      Prior=round(e$CRM.2$prior,3)
      n.patient=NULL
      total.tox=NULL
      ptox=round(e$CRM.2$ptox,3)
      ptoxL=round(e$CRM.2$ptoxL,3)
      ptoxU=round(e$CRM.2$ptoxU,3)
      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }
      for ( i in 1 : Study$Nb_Dl)
      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}

        total.tox=c(total.tox,res2)
      }
      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, n.patient=n.patient,
                                     total.tox=total.tox, ptox=ptox, ptoxL=ptoxL, ptoxU=ptoxU), envir = as.environment(pos))

      filef <- tclvalue(tkgetSaveFile())
      if (filef != "") {
        filesave=paste(filef,"xlsx",sep=".")
        pdf(paste(filef,"pdf",sep="."))
        GrapheResults()
        dev.off()
        pdf(paste(paste(filef,"2",sep=""),"pdf",sep="."))
        curvesIC()
        dev.off()
        wb <- createWorkbook()

        sheet1 <- addWorksheet(wb, sheetName="By patients")
        sheet2 <- addWorksheet(wb, sheetName="By dose level")
        style <- createStyle(fontSize = 12, fontColour = "black",
                             textDecoration = c("bold", "italic", "underline"),
                             halign = "center", valign = "center", border = "Bottom",
                             fgFill = "gray")

        setColWidths(wb, sheet1, cols=c(1,3), widths = 12.43)
        setColWidths(wb, sheet1, cols=2, widths = 14.43)
        setColWidths(wb, sheet2, cols=c(3,4), widths = 14.43)
        setColWidths(wb, sheet2, cols=c(1,2,5,6,7), widths = 10.43)

        writeDataTable(wb,sheet1,x=myRarray1)
        writeDataTable(wb,sheet2,x=myRarray2)

        addStyle(wb,sheet1,style,cols=1:3,rows=1)
        addStyle(wb,sheet2,style,cols=1:7,rows=1)

        saveWorkbook(wb, filesave, overwrite = T)
      }
    }

    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    l1<-tklabel(Resultas,text="")
    l2<-tklabel(Resultas,text="")
    l3<-tklabel(Resultas,text="")
    l4<-tklabel(Resultas,text="")


    Titre.1=tklabel(Resultas,text="Patient Summary",font=PoliceTitre)
    Titre.2=tklabel(Resultas,text="Dose level Summary",font=PoliceTitre)
    Titre.3=tklabel(Resultas,text="Graphics",font=PoliceTitre)
    Titre.4=tklabel(Resultas,text="Export results",font=PoliceTitre)

    button1=tkbutton(Resultas ,text="Results",width=20,command=Resultsbypatient,bg="cornflower blue")
    button2=tkbutton(Resultas ,text="Results",width=20,command=ResultsbyLevel,bg="cornflower blue")
    button3=tkbutton(Resultas ,text="Patient history",width=20,command=Export.Graph,bg="cornflower blue")
    button3b=tkbutton(Resultas ,text="Dose-tox curves",width=20,command=Export.CurvesIC,bg="cornflower blue")
    button4=tkbutton(Resultas ,text="Export",width=20,command=ExportRes,bg="cornflower blue")

    tkgrid(l1, row=0, column=0)
    tkgrid(Titre.1, row=1, column=1, sticky="w")
    tkconfigure(Titre.1, font=PoliceTitre)
    tkgrid(button1, row=1, column=2)

    tkgrid(l2, row=2, column=1)
    tkgrid(Titre.2, row=3, column=1, sticky="w")
    tkconfigure(Titre.2, font=PoliceTitre)
    tkgrid(button2, row=3, column=2)

    tkgrid(l3, row=4, column=1)
    tkgrid(Titre.3, row=5, column=1, sticky="w")
    tkconfigure(Titre.3, font=PoliceTitre)
    tkgrid(button3, row=5, column=2)
    tkgrid(button3b, row=5, column=3)

    tkgrid(l4, row=6, column=1)
    tkgrid(Titre.4, row=7, column=1, sticky="w")
    tkgrid(button4, row=7, column=2)
    tkconfigure(button4,padx=2)
    tkconfigure(Titre.4, font=PoliceTitre)

  }



  .include=function(...)
  {

    load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))

    Inc<-tktoplevel()
    tkwm.geometry(Inc, "+550+270")

    assign("mm", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(mm)
    Titre <- tklabel(mm,text="Treated dose level",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DL <- tclVar(Res$MTD)
    for ( i in 1: Study$Nb_Dl)
    {
      tkgrid(assign(paste('rDL',i,sep=''),tkradiobutton(mm,variable=rbValue.DL ,value=i, text=paste(i),font=PoliceGenerale)))
    }

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Inc.mm1)
    Titre <- tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i),font=PoliceGenerale)))
    }
    tkgrid(assign(paste('rDLT',2,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=2, text=paste("pending"))))

    .Sauv=function(...)
    {
      assign("DL", c(Res$PT.data[,1],as.numeric(tclvalue((rbValue.DL)))), envir = as.environment(pos))
      assign("DLT", c(Res$PT.data[,2],as.numeric(tclvalue((rbValue.DLT)))), envir = as.environment(pos))
      assign("patid", c(Res$PT.data[,3],Res$Pat.included+1), envir = as.environment(pos))
      assign("pndg", as.numeric (c(Res$PT.data[,4],as.numeric(tclvalue((rbValue.DLT)))==2)), envir = as.environment(pos))

      Res$PT.data <- data.frame(DL=DL,DLT=DLT,patid=patid,pndg=pndg)
      Res$Pat.included <- Res$Pat.included+1
      print(Res$PT.data)

      DATA=Res$PT.data[which(Res$PT.data$pndg==0),]
      print(length(DATA)!=0)
      if (nrow(DATA)!=0)
      {
        e$CRM.2<-dfcrm::crm(prior=Study$DL, target=Study$Target, tox=DATA[,2], level=DATA[,1], method = "bayes", conf.level = 0.95,
                            model = paste(Study$Model.n), scale=Study$Sd, intcpt=Study$Intcpt)

        Res$MTD <- e$CRM.2$mtd
      }
      CRM.2 <- e$CRM.2
      save(Res,Study,CRM.2,file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
      load(file =paste(paste(names_Study,'crmb',sep='-'),'RData',sep='.'))
      tkdestroy(Inc)
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }

      if(Res$Pat.included==1)
      {
        disabled.CRM()
        if (etatPrior==TRUE)
        {disabled.PriorCalib()}
        if (etatPrior==FALSE)
        {tkdestroy(PriorCal)}
      }


    }

    assign("Inc.Win_CRMB", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Inc.Win_CRMB,text="Validate",width=30,command=.Sauv,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_CRMB)

  }

  GrapheResults=function(...)
  {
    DATA=Res$PT.data[which(Res$PT.data$pndg==0),]
    n=length(DATA[,1])

    x=c(1:n)
    y=DATA[,1]
    z=DATA[,2]
    Data=data.frame(x=x,y=y,z=z)
    plot(x,y,pch=16,cex=2,col=(z+1),type='b',axes=FALSE,xlab="Patient number",ylab='Dose Level',main="Patients, Dose level and toxicity")
    axis(1, lwd = 2)
    axis(side=2,1:8, lwd = 2)
    legend("bottomright",inset = c(0.0, -0.19),  lwd=c(2.5,2.5),legend=c("Non Toxicity","Toxicity"), col = c(1,2), pch = 16,cex=0.8,xpd = NA)
  }

  Export.Graph=function()
  {
    GrapheResults()
  }

  curvesIC = function(...)
  {

    post.var <- e$CRM.2$post.var
    crit <- qnorm(0.5 + e$CRM.2$conf.level/2)
    doses=e$CRM.2$dosescaled
    Fempi=function(doses,beta){doses^(exp(beta))}
    Flogi=function(doses,beta){(1 + exp(-e$CRM.2$intcpt- exp(beta) * doses))^{-1}}
    Fmodel <- switch(e$CRM.2$model,empiric=Fempi, logistic=Flogi)
    plot(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),'l', lwd=2,xaxt="n",
         ylim=c(0, 1),xlab="Dose level", ylab="Proba. of DLT")
    axis(1, at = seq(1, length(e$CRM.2$prior), by = 1))
    abline(h=e$CRM.2$target,col="blue",lty=2,lwd=2)
    objtitle <-rbind("Dose-toxicity curve + 0.95_IC",paste(e$CRM.2$model, "model"))
    title(main=objtitle)
    legend('topright',c("Proba. of DLT", "IC(0.95)","target"),lty=c(1,1,2), col=c('black','red','blue'),lwd=2)
    points(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),lwd=2)
    if (post.var >= 0) {
      lb <- e$CRM.2$estimate - crit * sqrt(post.var)
      ub <- e$CRM.2$estimate + crit * sqrt(post.var)

      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,lb),col="red",lwd=2)
      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,ub),col="red",lwd=2)
    }else{print('Confidence interval not calculated')}
  }
  Export.CurvesIC=function()
  {
    curvesIC()
  }
  NewCRM=function()
  {

    tkdestroy(Win_CRMB)
    frameInitCRM()
    PriorCalib()
  }


  disabled.CRM=function()
  {

    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)
    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    Nb_Niv_Doses<- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }
    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding  ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Names of study:",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(labage, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,state="disabled",bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    .ReCap()
  }


  Open.CRM=function()
  {
    tkdestroy(Win_CRMB)
    frameInitCRM()


    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save, row=27, column=0, sticky="se")

    Nb_Niv_Doses<- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=5,  textvariable=n.iter,state="disabled"),) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Tsd.<-tklabel(Saisi, text="Normal s.deviation",font=PoliceGenerale)
    n.iter <- tclVar(Study$Sd)
    sd.<-tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tsd., row=6,column=2, sticky="w")
    tkgrid(sd., row=6,column=3, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale)
    n.iter <- tclVar(Study$Intcpt)
    intcpt.<-tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    Titre=tklabel(Saisi,text="Names of study",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(Titre, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=20,state="disabled")
    tkgrid(Save.rec, row=37, column=1, columnspan=2, rowspan=2)
    .ReCap()
    .ReCap.Resultat()
  }


  WordExport=function (dframe=NULL) {

    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}
    docx( ) %>%
      addFlexTable(dframe %>%
                     FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                               header.text.props = textBold(color = "white"),
                               add.rownames = TRUE ) %>%
                     setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
      writeDoc(file = "ResExport.docx")
    Save.word.infos <- tkmessageBox(title = "Word Export",
                                    message = "File ResExport.docx saved in the current working directory of the R proces", icon = "info", type = "ok")
  }

  ExcellExport=function (dframe=NULL) {
    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}

    filef_ <- tclvalue(tkgetSaveFile())
    if (filef_ != "") {
      filesave=paste(filef_,"xlsx",sep=".")

      wb_ <- createWorkbook()
      sheet1_ <- addWorksheet(wb_, sheetName="Simulation")

      style_ <- createStyle(fontSize = 12, fontColour = "black",
                            textDecoration = c("bold", "italic", "underline"),
                            halign = "center", valign = "center", border = "Bottom",
                            fgFill = "gray")

      setColWidths(wb_, sheet1_, cols=c(1,2,3,4,5), widths = 13.43)

      writeDataTable(wb_,sheet1_,x=dframe)

      addStyle(wb_,sheet1_,style_,cols=1:5,rows=1)

      saveWorkbook(wb_, filesave, overwrite = T)

    }
  }
  .RecapSim=function()
  {

    .Save_sim=function()
    {
      Study.file=function()
      {
        a00<-paste(StudyS$Name)
        a0=paste('Name of study',paste(StudyS$Name),sep=':')
        a1=paste('Method','CRMB',sep=':')
        a2='Continual Reassessment Method (CRM) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(StudyS$Name),'cbsim', sep='.'))
      }
      Study.file()
      ResSim <- e$ResSim
      save(ResSim,StudyS,file =paste(paste(StudyS$Name,'cbsim',sep='-'),'RData',sep='.'))
      Save.infos <- tkmessageBox(title = "Data storage Infos",
                                 message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

    assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.fr, row=0, column=0, rowspan=16)

    PriorRes=paste(round(e$ResSim$prior[1],3))
    TrueRes=paste(round(e$ResSim$PI[1],3))
    MTDRes=paste(round(e$ResSim$MTD[1],3))
    ToxRes=paste(round(e$ResSim$tox[1],3))
    LevelRes=paste(round(e$ResSim$level[1],3))
    for( i in 2 : length(e$ResSim$prior))
    {
      PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
      TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
      MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
      LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
      ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
    }


    NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    Titre1<-tklabel(Rec.fr,text="Results of the simulation")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
    aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
    aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
    aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
    aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
    aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
    aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
    aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
    aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
    aff_1.1<-tklabel(Rec.fr,text=paste("Continued reassessment method "))
    aff_1.2<-tklabel(Rec.fr,text=paste("bayesian approach (CRMB) "))
    aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
    aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
    aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
    aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
    aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
    aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
    aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
    aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
    aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
    aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
    aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
    aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

    tkgrid(aff_1.0, row=4, column=1,  sticky="w")
    tkgrid(aff_1.1, row=4, column=2,  sticky="w")
    tkgrid(aff_1.2, row=5, column=2,  sticky="w")
    tkconfigure(aff_1.0, font=Policeligne)
    tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
    tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

    tkgrid(aff_2.0, row=6, column=1, sticky="w")
    tkgrid(aff_2.1, row=6, column=2, sticky="w")
    tkconfigure(aff_2.0, font=Policeligne)
    tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

    tkgrid(aff_3.0, row=7, column=1, sticky="w")
    tkgrid(aff_3.1, row=7, column=2,  sticky="w")
    tkconfigure(aff_3.0, font=Policeligne )
    tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

    tkgrid(aff_4.0, row=8, column=1, sticky="w")
    tkgrid(aff_4.1, row=8, column=2,  sticky="w")
    tkconfigure(aff_4.0, font=Policeligne )
    tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

    tkgrid(aff_5.0, row=9, column=1,  sticky="w")
    tkgrid(aff_5.1, row=9, column=2,sticky="w")
    tkconfigure(aff_5.0, font=Policeligne )
    tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

    tkgrid(aff_6.0, row=10, column=1,  sticky="w")
    tkgrid(aff_6.1, row=10, column=2,  sticky="w")
    tkconfigure(aff_6.0, font=Policeligne)
    tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

    tkgrid(aff_7.0, row=11, column=1,  sticky="w")
    tkgrid(aff_7.1, row=11, column=2,  sticky="w")
    tkconfigure(aff_7.0, font=Policeligne )
    tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

    tkgrid(aff_11.0, row=12, column=1,  sticky="w")
    tkgrid(aff_11.1, row=12, column=2,  sticky="w")
    tkconfigure(aff_11.0, font=Policeligne )
    tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

    tkgrid(aff_8.0, row=13, column=1,  sticky="w")
    tkgrid(aff_8.1, row=13, column=2,  sticky="w")
    tkconfigure(aff_8.0, font=Policeligne )
    tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

    tkgrid(aff_9.0, row=14, column=1,  sticky="w")
    tkgrid(aff_9.1, row=14, column=2,  sticky="w")
    tkconfigure(aff_9.0, font=Policeligne )
    tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

    tkgrid(aff_10.0, row=15, column=1,  sticky="w")
    tkgrid(aff_10.1, row=15, column=2,  sticky="w")
    tkconfigure(aff_10.0, font=Policeligne )
    tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

    assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.save_, row=17, column=0)

    ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
    colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                         "Av. patient","Av. tox. ")
    WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                      command=function()ExcellExport(dframe=round(ResExp_,3)),bg="cornflower blue")
    tkgrid(WordExp_, row=1)
    blank<-tklabel(Rec.save_,text="      ")
    tkgrid(blank, row=2)
    SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,command=.Save_sim,bg="cornflower blue")
    tkgrid(SaveSim_, row=3)

  }
  disabled.SimCRM=function()
  {
    .RecapSimB=function()
    {

      .Save_sim=function()
      {
        Study.file=function()
        {
          a00<-paste(StudyS$Name)
          a0=paste('Name of study',paste(StudyS$Name),sep=':')
          a1=paste('Method','CRMB',sep=':')
          a2='Continual Reassessment Method (CRM) for Phase I Clinical Trials'
          datee<-paste(format(Sys.time(), "%A %d %B %Y"))
          a3=paste('Date of creation',datee)
          Data=data.frame(c(a00,a0,a1,a2,a3))
          write.table(Data,paste(paste(StudyS$Name),'cbsim', sep='.'))
        }
        Study.file()
        ResSim <- e$ResSim
        save(ResSim,StudyS,file =paste(paste(StudyS$Name,'cbsim',sep='-'),'RData',sep='.'))
        Save.infos <- tkmessageBox(title = "Data storage Infos",
                                   message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
      }
      PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
      PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

      assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=16)

      PriorRes=paste(round(e$ResSim$prior[1],3))
      TrueRes=paste(round(e$ResSim$PI[1],3))
      MTDRes=paste(round(e$ResSim$MTD[1],3))
      ToxRes=paste(round(e$ResSim$tox[1],3))
      LevelRes=paste(round(e$ResSim$level[1],3))
      for( i in 2 : length(e$ResSim$prior))
      {
        PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
        TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
        MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
        LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
        ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
      }


      NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
      tkgrid(NS, row=1, column=1,  sticky="w")
      tkconfigure(NS, font=PoliceEtude, foreground="white")

      Titre1<-tklabel(Rec.fr,text="Results of the simulation")
      tkgrid(Titre1, row=3, column=1)
      tkconfigure(Titre1, font=PoliceTitre)

      aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
      aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
      aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
      aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
      aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
      aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
      aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
      aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
      aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
      aff_1.1<-tklabel(Rec.fr,text=paste("Continued reassessment method "))
      aff_1.2<-tklabel(Rec.fr,text=paste("bayesian approach (CRMB) "))
      aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
      aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
      aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
      aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
      aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
      aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
      aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
      aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
      aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
      aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
      aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
      aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

      tkgrid(aff_1.0, row=4, column=1,  sticky="w")
      tkgrid(aff_1.1, row=4, column=2,  sticky="w")
      tkgrid(aff_1.2, row=5, column=2,  sticky="w")
      tkconfigure(aff_1.0, font=Policeligne)
      tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
      tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

      tkgrid(aff_2.0, row=6, column=1, sticky="w")
      tkgrid(aff_2.1, row=6, column=2, sticky="w")
      tkconfigure(aff_2.0, font=Policeligne)
      tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

      tkgrid(aff_3.0, row=7, column=1, sticky="w")
      tkgrid(aff_3.1, row=7, column=2,  sticky="w")
      tkconfigure(aff_3.0, font=Policeligne )
      tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

      tkgrid(aff_4.0, row=8, column=1, sticky="w")
      tkgrid(aff_4.1, row=8, column=2,  sticky="w")
      tkconfigure(aff_4.0, font=Policeligne )
      tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

      tkgrid(aff_5.0, row=9, column=1,  sticky="w")
      tkgrid(aff_5.1, row=9, column=2,sticky="w")
      tkconfigure(aff_5.0, font=Policeligne )
      tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

      tkgrid(aff_6.0, row=10, column=1,  sticky="w")
      tkgrid(aff_6.1, row=10, column=2,  sticky="w")
      tkconfigure(aff_6.0, font=Policeligne)
      tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

      tkgrid(aff_7.0, row=11, column=1,  sticky="w")
      tkgrid(aff_7.1, row=11, column=2,  sticky="w")
      tkconfigure(aff_7.0, font=Policeligne )
      tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

      tkgrid(aff_11.0, row=12, column=1,  sticky="w")
      tkgrid(aff_11.1, row=12, column=2,  sticky="w")
      tkconfigure(aff_11.0, font=Policeligne )
      tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

      tkgrid(aff_8.0, row=13, column=1,  sticky="w")
      tkgrid(aff_8.1, row=13, column=2,  sticky="w")
      tkconfigure(aff_8.0, font=Policeligne )
      tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

      tkgrid(aff_9.0, row=14, column=1,  sticky="w")
      tkgrid(aff_9.1, row=14, column=2,  sticky="w")
      tkconfigure(aff_9.0, font=Policeligne )
      tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

      tkgrid(aff_10.0, row=15, column=1,  sticky="w")
      tkgrid(aff_10.1, row=15, column=2,  sticky="w")
      tkconfigure(aff_10.0, font=Policeligne )
      tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

      assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.save_, row=17, column=0)

      ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
      colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                           "Av. patient","Av. tox. ")
      WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,command=function()ExcellExport(ResExp_),bg="cornflower blue")
      tkgrid(WordExp_, row=1)
      blank<-tklabel(Rec.save_,text="      ")
      tkgrid(blank, row=2)

      SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,state="disabled",bg="cornflower blue")
      tkgrid(SaveSim_, row=3)
    }

    tkdestroy(Saisi_)
    tkdestroy(m2_)
    tkdestroy(Run_)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)


    Nb_Niv_Doses<- StudyS$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (StudyS$Prior[i])
      n.itert<- tclVar (StudyS$True[i])


      tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8, textvariable=n.itert,state="disabled"),envir = as.environment(pos)),row=r,column=1)

      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=3)

    }


    SliderValue_=tclVar(StudyS$Nb_Dl)
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Target)
    Target_<-tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")


    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Npat)
    n_<-tkentry(Saisi_, width=5, textvariable=n.iter ,state="disabled")
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Nsim)
    Nsim_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$X0)
    X0_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Seed)
    Seed_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_,state="disabled") ; rb2.1_ <- tkradiobutton(Saisi_,state="disabled")
    if (StudyS$Model == "logistic") {
      rbValue.M_ <- tclVar(1)
      n.intcpt <- tclVar(StudyS$Intcpt)}
    if (StudyS$Model == "empiric") {
      rbValue.M_ <- tclVar(2)
      n.intcpt <- tclVar("NA")}
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal s.deviation", font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Sd)
    sd_<-tkentry(Saisi_, width=7, textvariable=n.iter, state="disabled")
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept", font=PoliceGenerale)
    intcpt_<-tkentry(Saisi_, width=7, textvariable=n.intcpt, state="disabled")
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_, state="disabled") ;
    const2_ <- tkradiobutton(Saisi_, state="disabled")
    rbValue.const_ <- tclVar(as.numeric(StudyS$Constrain))
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=9, column=0, sticky="w")
    tkgrid(const1_, row=9,  column=1, sticky="w")
    tkgrid(const2_, row=10,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(StudyS$Name)
    names.S_<-tkentry(Saisi_, width=20,textvariable=Nam_,state="disabled")
    tkgrid(labage_, row=11,column=0, sticky="w")
    tkgrid(names.S_, row=11,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,state="disabled",bg="cornflower blue")
    tkgrid(RunSim_, row=1)

    .RecapSimB()
  }

  SimCRM=function()
  {

    tkdestroy(Win_CRMB)
    frameInitSIM()

    .Crea.f_=function(...)
    {
      Nb_Niv_Doses_<-as.numeric(tclvalue((SliderValue_)))
      assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)

      for ( i in 1: Nb_Niv_Doses_)
      {
        r=17 + i
        tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=1)

        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=3)
      }
      if( Nb_Niv_Doses_!=8)
      {
        for ( i in (Nb_Niv_Doses_+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)

          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=2)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=3)
        }
      }
    }

    .Param.c_ = function(...)
    {
      if (as.numeric(tclvalue((rbValue.M_)))==2)
      {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt_", tkentry(Saisi_, width=7, state="disabled"), envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")
      }
      else {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
        assign("intcpt_",tkentry(Saisi_, width=7),envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")
      }
    }


    .Simulation_=function (...)
    {
      .Sauvgarde.Study_=function(...)
      {
        Nb_Dl=as.numeric(tclvalue(SliderValue_))
        prior=NULL
        true=NULL
        if (Nb_Dl==2) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))))}
        if (Nb_Dl==3) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))))}
        if (Nb_Dl==4) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))))}
        if (Nb_Dl==5) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))))}
        if (Nb_Dl==6) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))))}
        if (Nb_Dl==7) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))))}
        if (Nb_Dl==8) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))),as.numeric(tclvalue(tkget(valt8))))}

        seed=1;target=NULL;Model=NULL
        npat=NULL;nsim=1;x0=1
        target=as.numeric(tclvalue(tkget((Target_))))

        Model=as.numeric(tclvalue((rbValue.M_)))
        Const=as.logical(as.numeric(tclvalue((rbValue.const_))))
        npat=as.numeric(tclvalue(tkget(n_)))
        nsim=as.numeric(tclvalue(tkget((Nsim_))))
        x0=as.numeric(tclvalue(tkget((X0_))))
        seed=as.numeric(tclvalue(tkget((Seed_))))
        sdv=as.numeric(tclvalue(tkget((sd_))))
        if (Model == 1) {model=as.character("logistic");intcp=as.numeric(tclvalue(tkget(intcpt_)))}
        if (Model == 2) {model=as.character("empiric");intcp=NULL}
        res.users <- 1
        if (target>0.55) cat("\n Warning: Target DLT rate too high")
        if (target<0.15) cat("\n Warning: Target DLT rate too low")
        if (npat <=0) stop('Number of patients to be enrolled <=0')
        if (npat > 100) cat("\n Warning: Number of patients to be enrolled > 100")
        if ((x0 <1)|(x0 > Nb_Dl)|(x0%%1 != 0)) stop(paste('Starting dose level incorrect, enter an integer between 1 and',Nb_Dl))
        if (nsim < 100) cat("\n Warning: Low number of simulations")
        check_nsim <- function (){
          res.users <- 2
          while (res.users != 0 & res.users != 1) {
            cat("\n Warning: Large number of simulations, continue?  y or n")
            yn <- readLines(n=1)
            if(yn == "y" | yn == "Y" | yn== "yes" | yn == "Yes"){res.users <- 1;}
            if(yn == "n" | yn == "N" | yn== "no" | yn == "No"){res.users <- 0;}
          }
          return(res.users)
        }

        if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
        if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
        if (any(true <= 0) | any(true >= 1)) stop('True probabilities must be within ]0,1[ ')
        if (is.unsorted(true, strictly = TRUE)) stop('True probabilities should be monotonically increasing')
        if (nsim > 10000)  {res.users <- check_nsim()}
        assign("Names_Study", tclvalue(tkget(names.S_)), envir = as.environment(pos))

        assign("StudyS",list(Name=paste(Names_Study),Nb_Dl=Nb_Dl,True=true,Prior=prior,Target=target,
                             Model=model,Npat=npat,Nsim=nsim,X0=x0, Intcpt=intcp, Sd=sdv,
                             Constrain=Const, Seed=seed), envir = as.environment(pos))
        if (res.users == 1) {
          cat("\n Submission in progress... Please wait... ")
          e$ResSim <- dfcrm::crmsim(PI=StudyS$True, prior=StudyS$Prior,target=StudyS$Target,n=StudyS$Npat,
                                    x0=StudyS$X0,nsim=StudyS$Nsim,method="bayes",model=StudyS$Model,count=FALSE,
                                    intcpt=StudyS$Intcpt, scale=StudyS$Sd, restrict=StudyS$Constrain, seed=StudyS$Seed)

          .RecapSim()
          tk2notetab.select(net.CRMB_, "Results")
        }
      }
      .Sauvgarde.Study_()

    }


    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    SliderValue_=tclVar('8')

    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",command=.Crea.f_)

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    Target_<-tkentry(Saisi_, width=5)

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    l<-tklabel(Saisi_,text="            ")
    tkgrid(l, row=2, column=3)

    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    n_<-tkentry(Saisi_, width=5)
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    Nsim_<-tkentry(Saisi_, width=10)
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    X0_<-tkentry(Saisi_, width=10)
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    Seed_<-tkentry(Saisi_, width=10)
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_, command=.Param.c_) ; rb2.1_ <- tkradiobutton(Saisi_, command=.Param.c_)
    rbValue.M_ <- tclVar(1)
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal s.deviation",font=PoliceGenerale)
    sd_<-tkentry(Saisi_, width=7)
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
    assign("intcpt_",tkentry(Saisi_, width=7),envir = as.environment(pos))
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_) ; const2_ <- tkradiobutton(Saisi_)
    rbValue.const_ <- tclVar(1)
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=9, column=0, sticky="w")
    tkgrid(const1_, row=9,  column=1, sticky="w")
    tkgrid(const2_, row=10,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    names.S_<-tkentry(Saisi_, width=18)

    tkgrid(labage_, row=11,column=0, sticky="w")
    tkgrid(names.S_, row=11,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,command=.Simulation_,bg="cornflower blue")
    tkgrid(RunSim_, row=1)

  }

  save_Qf2=function(){ tkmessageBox(title = "Data storage Infos",
                                    message = "Data are automatically saved after each inclusion", icon = "info", type = "ok")}
  save_Qf=function(){ tkmessageBox(title = "Data storage Infos",
                                   message = "Please use the button save study available in interactive CRM or simulator to save your data", icon = "info", type = "ok")}
  about=function(){ tkmessageBox(title = "Information",
                                 message = "Continual Reassessment Method bayesian Interface 2017",
                                 icon = "info", type = "ok")}

  Open.help=function() {browseURL("https://cran.r-project.org/package=dfcrm", browser=getOption("browser"),
                                  encodeIfNeeded = FALSE) }

  frameInit=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))

    assign("Win_CRMB", tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_CRMB, "630x472")
    tkwm.geometry(Win_CRMB, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRMB, ilogo)
    tktitle(Win_CRMB) <- "GUI CRMB"
    tkpack.propagate(Win_CRMB, FALSE)

    topMenu <- tk2menu(Win_CRMB)
    tkconfigure(Win_CRMB, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)
    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command= function(){ tkdestroy(Win_CRMB);frameInit()})
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRMB))

  }

  frameInitCRM=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))
    assign("Win_CRMB",tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_CRMB, "680x555")
    tkwm.geometry(Win_CRMB, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRMB, ilogo)
    tktitle(Win_CRMB) <- "GUI CRMB"
    tkpack.propagate(Win_CRMB, FALSE)

    topMenu <- tk2menu(Win_CRMB)
    tkconfigure(Win_CRMB, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)
    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command= function(){ tkdestroy(Win_CRMB);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf2)
    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRMB))

    assign("net.CRMB", tk2notebook(Win_CRMB, tabs = c("Prior calibration","Input parameters","Include","Results")),
           envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.CRMB , fill = "both", expand = 1)

    assign("Include", tk2notetab(net.CRMB , "Include"), envir = as.environment(pos))
    tkpack.propagate(Include, FALSE)

    assign("Resultas", tk2notetab(net.CRMB , "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas, FALSE)

    assign("Required", tk2notetab(net.CRMB , "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required, FALSE)

    assign("PriorCal", tk2notetab(net.CRMB , "Prior calibration"), envir = as.environment(pos))
    tkpack.propagate(PriorCal, FALSE)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)

    assign("Saisi.save" ,tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Rec.Win_CRMB", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.Win_CRMB, row=9, column=2, rowspan=16)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, columnspan=3, rowspan=16)

    assign("halfwidthPrior", NULL, envir = as.environment(pos))
    assign("nuPrior", NULL, envir = as.environment(pos))
    assign("etatPrior", FALSE, envir = as.environment(pos))

  }


  frameInitSIM=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))
    assign("Win_CRMB",tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_CRMB, "660x595")
    tkwm.geometry(Win_CRMB, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRMB, ilogo)
    tktitle(Win_CRMB) <- "GUI CRMB"
    tkpack.propagate(Win_CRMB, FALSE)

    topMenu <- tk2menu(Win_CRMB)
    tkconfigure(Win_CRMB, menu=topMenu)

    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command= function(){ tkdestroy(Win_CRMB);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRMB))

    assign("net.CRMB_", tk2notebook(Win_CRMB, tabs = c("Input parameters","Results")), envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.CRMB_ , fill = "both", expand = 1)


    assign("Resultas_", tk2notetab(net.CRMB_ , "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas_, FALSE)

    assign("Required_", tk2notetab(net.CRMB_ , "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required_, FALSE)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkpack.propagate(m2_, FALSE)

  }

  frameInit()
}

.CRML=function(pos=1, e)
{
  e$CRM.2=NULL
  tclRequire("Tktable")
  remplireRequid=function()
  {
    .Sauvgarde.Study=function(...)
    {
      Nb_Dl=as.numeric(tclvalue(SliderValue))
      DL=NULL
      if (Nb_Dl==2) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))}
      if (Nb_Dl==3) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))}
      if (Nb_Dl==4) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))}
      if (Nb_Dl==5) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))}
      if (Nb_Dl==6) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))}
      if (Nb_Dl==7) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))}
      if (Nb_Dl==8) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))}

      Target=as.numeric(tclvalue(tkget((Target))))

      Model=as.numeric(tclvalue((rbValue.M)))


      if (Model==1){Model.n="logistic"; intcpt=as.numeric(tclvalue(tkget(intcpt.)))}
      if (Model==2){Model.n="empiric"; intcpt=NULL}

      Stop=as.numeric(tclvalue(rbValue.SC))

      N_Pat=as.numeric(tclvalue(tkget(NBpat)))

      N_pat_Level=as.numeric(tclvalue(tkget(NBpat)))

      if ((Stop == 1) & (N_Pat <=0)) stop('Number of patients to be enrolled in the study <=0')
      if ((Stop == 1) & (N_Pat > 100)) cat("\n Warning: Number of patients to be enrolled in the study > 100")
      if ((Stop == 2) & (N_Pat <=0)) stop('Number of patients by dose level  <=0')
      if ((Stop == 2) & (N_Pat > 20)) cat("\n Warning: Large number of patients by dose level")

      assign("names_Study",'', envir = as.environment(pos))
      assign("names_Study", tclvalue(tkget(names.S)), envir = as.environment(pos))

      assign("Study", list(name=paste(names_Study),Nb_Dl=Nb_Dl,DL=DL,Target=Target,Model=Model,Model.n=Model.n,
                           Stop=Stop,N_Pat=N_Pat,N_pat_Level=N_pat_Level,halfwidthPrior=halfwidthPrior,
                           nuPrior=nuPrior, Intcpt=intcptPrior, Sd=sdPrior), envir = as.environment(pos))

      differ=abs(Study$DL-Study$Target)
      MTD=1
      Pat_include=0
      PT.data=NULL

      assign("Res", list(MTD=MTD,Pat.included=Pat_include,PT.data=PT.data), envir = as.environment(pos))
      save(Res,Study,file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
      .ReCap()

      Study.file=function()
      {
        a00=paste(names_Study)
        a0=paste('Name of study',paste(names_Study),sep=':')
        a1=paste('Method','CRM',sep=':')
        a2='Continual Reassessment Method (CRML) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(names_Study),'crml', sep='.'))
      }
      Study.file()
      tk2notetab.select(net.CRML , "Include")
    }

    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)
    tk2notetab.select(net.CRML , "Input parameters")

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save, row=27, column=0, sticky="se")
    Nb_Niv_Doses <- nlevel
    priorProbaf <- c()

    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (tkget(eval(parse(text = paste('valp',i,sep='')))))
      priorProbaf <- c(priorProbaf,as.numeric(tclvalue(n.iter)))
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)),sticky="w",row=r,column=1)
    }

    if (any(priorProbaf <= 0) | any(priorProbaf >= 1)) stop('Prior probabilities must be within ]0,1[ ')
    if (is.unsorted(priorProbaf, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')

    SliderValue=tclVar(nlevel)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1,
                            state="disabled",orient="horiz")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")


    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(TargetPrior)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")


    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi) ; rb2 <- tkradiobutton(Saisi)
    rbValue.SC <- tclVar(1)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NBpat<-tkentry(Saisi, width=10)
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Models",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi) ; rb2.1 <- tkradiobutton(Saisi)
    rbValue.M <- tclVar(modelPrior)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic", state="disabled", font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric", state="disabled",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Tsd.<-tklabel(Saisi, text="Normal s.deviation",font=PoliceGenerale)
    n.iter <- tclVar(sdPrior)
    sd.<-tkentry(Saisi,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=8,column=0, sticky="w")
    tkgrid(sd., row=8,column=1, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(intcptPrior)
    }else {tclVar("NA")}
    intcpt.<-tkentry(Saisi, textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tintcpt., row=8,column=2, sticky="w")
    tkgrid(intcpt., row=8,column=3, sticky="w")


    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    Nam<- tclVar()
    names.S<-tkentry(Saisi, width=20)

    tkgrid(labage, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,command=.Sauvgarde.Study,bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    assign("etatPrior", TRUE, envir = as.environment(pos))
  }



  disabled.PriorCalib=function(...)
  {
    if(is.tkwin(affCAL)){tkdestroy(affCAL)}
    if(is.tkwin(SaisiCal)){tkdestroy(SaisiCal)}
    if(is.tkwin(Saisi.saveCal )){tkdestroy(Saisi.saveCal )}

    assign("affCal.crmb", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCal.crmb, row=10)

    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)
    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))

    SliderValue.CAL=tclVar(Study$Nb_Dl)
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    for ( i in 1: Study$Nb_Dl)
    {
      r=10 + i
      assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
      n.iter=tclVar(tkget(eval(parse(text = paste('val',i,sep='')))))
      tkgrid(assign(paste('lab',i,sep=''),tklabel(affCal.crmb,text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(affCal.crmb,state="disabled", width=8,textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=1)
    }

    if(Study$Nb_Dl!=8)
    {
      for ( i in (Study$Nb_Dl+1):8)
      {
        r=10 + i
        tkgrid(assign(paste('lab',i,sep=''),tklabel(affCal.crmb, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
        tkgrid(assign(paste('val',i,sep=''),tkentry(affCal.crmb, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
      }
    }

    Titre=tklabel(SaisiCal,text="Target DLT rate",font=PoliceGenerale)
    n.iter=tclVar(round(TargetPrior,3))
    Target.cal<-tkentry(SaisiCal,textvariable=n.iter,state="disabled", width=5)

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    n.iter=tclVar(halfwidthPrior)
    halfwidth<-tkentry(SaisiCal, width=10,textvariable=n.iter,state="disabled")
    tkgrid(lab_halfwidth, row=3,column=0, sticky="w")
    tkgrid(halfwidth, row=3,column=1, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal,state="disabled") ; rb2.1CAL <- tkradiobutton(SaisiCal,state="disabled")
    assign("rbValue.MCAL", tclVar(modelPrior),envir = as.environment(pos))
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    n.iter=tclVar(nuPrior)
    nu<-tkentry(SaisiCal, width=10,textvariable=n.iter,state="disabled")
    tkgrid(lab_nu, row=6,column=0, sticky="w")
    tkgrid(nu, row=6,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="Normal s.deviation",font=PoliceGenerale)
    n.iter=tclVar(Study$Sd)
    sd.<-tkentry(SaisiCal,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=7,column=0, sticky="w")
    tkgrid(sd., row=7,column=1, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(Study$Intcpt)}
    else {tclVar("NA")}
    assign("intcpt.",tkentry(SaisiCal, textvariable=n.iter, width=10, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,bg = "cornflower blue",state="disabled")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,bg="cornflower blue",state="disabled")
    tkgrid(Validation, row=2)
  }


  PriorCalib=function(...)
  {
    .Crea.f.CAL=function(...)
    {
      assign("TargetPrior",as.numeric(tclvalue(tkget((Target.cal)))), envir = as.environment(pos))
      assign("halfwidthPrior",as.numeric(tclvalue(tkget((halfwidth)))), envir = as.environment(pos))
      assign("nuPrior",as.numeric(tclvalue(tkget(nu))), envir = as.environment(pos))
      assign("nlevel",as.numeric(tclvalue(tkget((slider_Nb_DL.CAL)))), envir = as.environment(pos))
      assign("modelPrior",as.numeric(tclvalue(((rbValue.MCAL)))), envir = as.environment(pos))
      assign("sdPrior",as.numeric(tclvalue(tkget((sd.)))), envir = as.environment(pos))
      assign("intcptPrior",as.numeric(tclvalue(tkget((intcpt.)))), envir = as.environment(pos))
      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))
      assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)

      if (TargetPrior>0.55) cat("\n Warning: Target DLT rate too high")
      if (TargetPrior<0.15) cat("\n Warning: Target DLT rate too low")
      if ((halfwidthPrior<=0)|(halfwidthPrior>0.5)) stop('Halfwidth of the indifference intervals incorrect!')
      if ((halfwidthPrior<=0.5)&(halfwidthPrior>0.2)) cat("\n Warning: Try to reduce the value of the halfwidth of the indifference intervals ")
      if ((nuPrior<1)|(nuPrior>Nb_Niv_DosesCAL)|(nuPrior%%1 != 0)) stop(paste('Prior guess of MTD incorrect, enter an integer between 1 and',Nb_Niv_DosesCAL))

      if (modelPrior==1)
      {
        e$prior <- dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior, nlevel=8, model="logistic", intcpt=intcptPrior)
      }

      if (modelPrior==2)
      {
        e$prior <- dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior, nlevel=8, model="empiric")
      }


      for ( i in 1: Nb_Niv_DosesCAL)
      {
        r=10 + i
        assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
        n.iter=tclVar(round(e$prior[i],3))
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,textvariable=n.iter),envir = as.environment(pos)),row=r,column=1)
      }

      if( Nb_Niv_DosesCAL!=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i
          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }
    }

    f.CALPrior=function(...)
    {
      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))

      for ( i in 1: Nb_Niv_DosesCAL)
      {
        r=10 + i
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8),envir = as.environment(pos)),row=r,column=1)
      }


      if(Nb_Niv_DosesCAL !=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i

          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }

    }

    .Param.choice = function(...)
    {
      if (as.numeric(tclvalue((rbValue.MCAL)))==2)
      {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt.", tkentry(SaisiCal, width=10, state="disabled"),envir = as.environment(pos))
        tkgrid(Tintcpt., row=5,column=2, sticky="w")
        tkgrid(intcpt., row=5,column=3, sticky="w")
      }
      else {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale)
        assign("intcpt.",tkentry(SaisiCal, width=10),envir = as.environment(pos))
        tkgrid(Tintcpt., row=5,column=2, sticky="w")
        tkgrid(intcpt., row=5,column=3, sticky="w")
      }
    }

    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)
    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)
    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))

    SliderValue.CAL=tclVar('8')
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz",command=f.CALPrior)
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    Titre=tklabel(SaisiCal,text="Target DLT rate ",font=PoliceGenerale)
    Target.cal<-tkentry(SaisiCal, width=5)
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    halfwidth<-tkentry(SaisiCal, width=10)
    tkgrid(lab_halfwidth, row=3,column=0, sticky="w", columnspan=2)
    tkgrid(halfwidth, row=3,column=2, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice)
    rb2.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice)
    assign("rbValue.MCAL", tclVar(1), envir = as.environment(pos))
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="Normal s.deviation",font=PoliceGenerale)
    sd.<-tkentry(SaisiCal, width=10)
    tkgrid(Tsd., row=4,column=2, sticky="w")
    tkgrid(sd., row=4,column=3, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    assign("intcpt.", tkentry(SaisiCal, width=10), envir = as.environment(pos))
    tkgrid(Tintcpt., row=5,column=2, sticky="w")
    tkgrid(intcpt., row=5,column=3, sticky="w")


    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    nu<-tkentry(SaisiCal, width=10)
    tkgrid(lab_nu, row=7,column=0, sticky="w")
    tkgrid(nu, row=7,column=1, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,command=.Crea.f.CAL,bg="cornflower blue")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,command=remplireRequid,bg="cornflower blue")
    tkgrid(Validation, row=2)
  }


  loads <- function()
  {
    loads.infos <- tkmessageBox(title = "Data loading Infos",
                                message = "Select a file.crml or file.clsim", icon = "info", type = "ok")

    filef <- tclvalue(tkgetOpenFile())
    if (filef != "") {
      stu=read.table(filef,header=TRUE)
      assign("names_Study", paste(stu[1,]), envir = as.environment(pos))
      extension<-str_sub(filef,-4)
    }
    if (extension == "crml") {
      load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'),envir = as.environment(pos))

      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      e$CRM.2 <- CRM.2
      Open.CRM()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          tkdestroy(Include)
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            tkdestroy(Include)
          }
        }
      }
    }
    if (extension == "lsim") {
      load(file =paste(paste(names_Study,'clsim',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      e$ResSim <- ResSim
      assign("StudyS", StudyS, envir = as.environment(pos))
      tkdestroy(Win_CRML)
      frameInitSIM()
      disabled.SimCRM()

    }
  }

  pending_listing=function()
  {

    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
    dataa=Res$PT.data[which(Res$PT.data$pndg==1),]

    if (nrow(dataa)==0 )
    {
      msg <- paste("No patient pending",sep="")
      tkmessageBox(message=msg)
    }

    if (nrow(dataa)>=1)
    {
      assign("tt.pend", tktoplevel(), envir = as.environment(pos))
      tkwm.geometry(tt.pend, "+650+400")
      assign("tl", tklistbox(tt.pend,height=4,selectmode="single",background="white"), envir = as.environment(pos))
      tkgrid(tklabel(tt.pend,text="Select a patient",font=PoliceGenerale))
      tkgrid(tl)
      numPatP=paste("Patient N'", as.character(dataa$patid),sep="")
      for (i in (1:length(numPatP)))
      {
        tkinsert(tl,"end",numPatP[i])
      }

      tkselection.set(tl,0)
      OK.but <- tkbutton(tt.pend,text="  OK  ",command=.include.pending,bg="cornflower blue")
      tkgrid(OK.but)
      tkfocus(tt.pend)
    }
  }

  .include.pending <- function()
  {
    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
    data.pinding=Res$PT.data[which(Res$PT.data$pndg==1),]
    data.inclus=Res$PT.data[which(Res$PT.data$pndg==0),]
    numPatP=as.character(data.pinding$patid)
    assign("Choice", as.numeric(numPatP[as.numeric(tkcurselection(tl))+1]), envir = as.environment(pos))

    Inc=tktoplevel()
    tkwm.geometry(Inc, "+650+400")

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Inc.mm1)
    Titre<-tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i))))
    }

    .Sauv2=function(...)
    {

      assign("DLT", as.numeric(tclvalue((rbValue.DLT))), envir = as.environment(pos))
      Newpat=data.pinding[which(data.pinding$patid==Choice),]

      Newpat$DLT=DLT
      Newpat$pndg=0
      DATA=rbind(data.inclus,Newpat)

      if (nrow(DATA)!=0)
      {
        if (sum(DATA[,2])==0)
        {
          numpat=length(DATA[,2])+1
          if (numpat!=1)
          {
            print('numpat')
            print(numpat)
            temp <- (numpat-1)/3
            print('temp')
            print(temp)
            if (temp==trunc(temp))
            {
              print('temp==trunc(temp)')
              print(trunc(temp))
              Res$MTD <- DATA$DL[numpat-1]+1
              if (Res$MTD>Study$Nb_Dl){Res$MTD <-DATA$DL[numpat-1]}
            }
            else
            {Res$MTD <-DATA$DL[numpat-1]}
          }
        }
        else
        {
          environment()
          e$CRM.2 <- dfcrm::crm(prior=Study$DL, target=Study$Target, tox=DATA[,2],level=DATA[,1], conf.level = 0.95,
                                method = "mle", model = paste(Study$Model.n), intcpt=Study$Intcpt, scale=Study$Sd)
          Res$MTD<-e$CRM.2$mtd
        }

      }

      Res$PT.data[which(Res$PT.data$patid==Choice),]$pndg=0
      Res$PT.data[which(Res$PT.data$patid==Choice),]$DLT=DLT
      if (nrow(DATA)!=0) {
        if (sum(DATA[,2])==0) {
          save(Res,Study,file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))}
        else {CRM.2<- e$CRM.2; save(Res,Study,CRM.2,file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))}
      }

      load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))

      tkdestroy(Inc)
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }

      if(Res$Pat.included==1)
      {
        disabled.CRM()
        if (etatPrior==TRUE)
        {disabled.PriorCalib()}
        if (etatPrior==FALSE)
        {tkdestroy(PriorCal)}
      }

    }

    assign("Inc.Win_CRML", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Inc.Win_CRML,text="Validate",width=30,command=.Sauv2,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_CRML)
    tkdestroy (tt.pend)

  }

  .ReCap=function(...)
  {

    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Continued reassessment method-likelihood approach (CRML) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(Study$Model))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(Study$Stop))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_CRML", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Rec.Win_CRML,text="New patient",width=30,command=.include,bg="cornflower blue")
    Save.pend=tkbutton(Rec.Win_CRML,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

    tkgrid(Rec.Win_CRML, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    tkgrid(Save.pend, row=0, column=1)
  }

  disabled.ReCap=function(...)
  {

    tkdestroy(Rec.mm0)
    tkdestroy(Rec.Win_CRML)

    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Continued reassessment method-likelihood approach (CRML) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(Study$Model))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(Study$Stop))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"),font=PoliceGenerale)
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included),font=PoliceGenerale)

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"),font=PoliceGenerale)
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD),font=PoliceGenerale)

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    DATA=Res$PT.data[which(Res$PT.data$pndg==1),]
    if (nrow(DATA)==0)
    {
      assign("Rec.Win_CRML", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_CRML,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_CRML,text="Pending patient",width=30,state="disabled",bg="cornflower blue")
      tk2notetab.select(net.CRML ,"Results")

      tkgrid(Rec.Win_CRML, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
    }
    if (nrow(DATA)!=0)
    {
      assign("Rec.Win_CRML", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_CRML,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_CRML,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

      tkgrid(Rec.Win_CRML, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
    }
  }

  .ReCap.Resultat=function(...)
  {

    Resultsbypatient=function()
    {
      assign("myRarray1", data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2]),
             envir = as.environment(pos))
      X=colnames(myRarray1)
      myRarray=rbind(X,myRarray1)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="25",background="white")
      tkgrid(t.d, row=2, column=0)

    }
    ResultsbyLevel=function()
    {
      if (!is.null(e$CRM.2)){
        Prior=round(e$CRM.2$prior,3)
        n.patient=NULL
        total.tox=NULL
        ptox=round(e$CRM.2$ptox,3)
        ptoxL=round(e$CRM.2$ptoxL,3)
        ptoxU=round(e$CRM.2$ptoxU,3)
        for ( i in 1 : Study$Nb_Dl)
        {
          res1=sum(Res$PT.data[,1]==i)
          n.patient=c(n.patient,res1)
        }
        for (i in 1 : Study$Nb_Dl)
        {
          res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
          if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}
          total.tox=c(total.tox,res2)
        }

        assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, n.patient=n.patient,
                                       total.tox=total.tox, ptox=ptox, ptoxL=ptoxL, ptoxU=ptoxU), envir = as.environment(pos))
        X=colnames(myRarray2)
        myRarray=rbind(X,myRarray2)

        a=dim(myRarray)[2]-1
        b=dim(myRarray)[1]-1

        for (i in (0:b))
          for (j in (0:a))
            .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

        Resbypat<-tktoplevel()
        t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="17",background="white")
        tkgrid(t.d, row=2, column=0)
      } else {print('Results are displayed once toxicity is observed')}
    }

    ExportRes=function()
    {
      myRarray1 <- data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],
                              Toxicity=Res$PT.data[,2])
      if (!is.null(e$CRM.2)) {
        Prior=round(e$CRM.2$prior,3)
        n.patient=NULL
        total.tox=NULL
        ptox=round(e$CRM.2$ptox,3)
        ptoxL=round(e$CRM.2$ptoxL,3)
        ptoxU=round(e$CRM.2$ptoxU,3)
        for ( i in 1 : Study$Nb_Dl)
        {
          res1=sum(Res$PT.data[,1]==i)
          n.patient=c(n.patient,res1)
        }
        for ( i in 1 : Study$Nb_Dl)
        {
          res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
          if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}

          total.tox=c(total.tox,res2)
        }
        assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, n.patient=n.patient,total.tox=total.tox,
                                       ptox=ptox, ptoxL=ptoxL, ptoxU=ptoxU), envir = as.environment(pos))

        Da=data.frame(Study[1],Study[2],Study[4],Study[5],method="Continued reassessment method-likelihood approach (CRML) ",Res[2],Res[1])
        DD=t(Da)
        rownames(DD)=c("Name of study","Number of dose level","Target DLT rate","Model","Statistical method","Number of patients included", "Estimated MTD")
        colnames(DD)=""
        filef <- tclvalue(tkgetSaveFile())
        if (filef != "") {
          filesave=paste(filef,"xlsx",sep=".")
          pdf(paste(filef,"pdf",sep="."))
          GrapheResults()
          dev.off()
          pdf(paste(paste(filef,"2",sep=""),"pdf",sep="."))
          curvesIC()
          dev.off()
          wb <- createWorkbook()
          sheet1 <- addWorksheet(wb, sheetName="By patients")
          sheet2 <- addWorksheet(wb, sheetName="By dose level")
          style <- createStyle(fontSize = 12, fontColour = "black",
                               textDecoration = c("bold", "italic", "underline"),
                               halign = "center", valign = "center", border = "Bottom",
                               fgFill = "gray")

          setColWidths(wb, sheet1, cols=c(1,3), widths = 12.43)
          setColWidths(wb, sheet1, cols=2, widths = 14.43)
          setColWidths(wb, sheet2, cols=c(3,4), widths = 14.43)
          setColWidths(wb, sheet2, cols=c(1,2,5,6,7), widths = 10.43)

          writeDataTable(wb,sheet1,x=myRarray1)
          writeDataTable(wb,sheet2,x=myRarray2)

          addStyle(wb,sheet1,style,cols=1:3,rows=1)
          addStyle(wb,sheet2,style,cols=1:7,rows=1)

          saveWorkbook(wb, filesave, overwrite = T)
        }
      }else {
        filef <- tclvalue(tkgetSaveFile())
        if (filef != "") {
          filesave=paste(filef,"xlsx",sep=".")
          wb <- createWorkbook()
          sheet1 <- addWorksheet(wb, sheetName="By patients")
          style <- createStyle(fontSize = 12, fontColour = "black",
                               textDecoration = c("bold", "italic", "underline"),
                               halign = "center", valign = "center", border = "Bottom",
                               fgFill = "gray")

          setColWidths(wb, sheet1, cols=c(1,3), widths = 12.43)
          setColWidths(wb, sheet1, cols=2, widths = 14.43)
          writeDataTable(wb,sheet1,x=myRarray1)
          addStyle(wb,sheet1,style,cols=1:3,rows=1)
          saveWorkbook(wb, filesave, overwrite = T)
        }
      }
    }

    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    l1<-tklabel(Resultas,text="")
    l2<-tklabel(Resultas,text="")
    l3<-tklabel(Resultas,text="")
    l4<-tklabel(Resultas,text="")


    Titre.1=tklabel(Resultas,text="Patient Summary",font=PoliceTitre)
    Titre.2=tklabel(Resultas,text="Dose level Summary",font=PoliceTitre)
    Titre.3=tklabel(Resultas,text="Graphic",font=PoliceTitre)
    Titre.4=tklabel(Resultas,text="Export results",font=PoliceTitre)

    button1=tkbutton(Resultas ,text="Results",width=20,command=Resultsbypatient,bg="cornflower blue")
    button2=tkbutton(Resultas ,text="Results",width=20,command=ResultsbyLevel,bg="cornflower blue")
    button3=tkbutton(Resultas ,text="Graphic",width=20,command=Export.Graph,bg="cornflower blue")
    button3b=tkbutton(Resultas ,text="Dose-tox curves",width=20,command=Export.CurvesIC,bg="cornflower blue")
    button4=tkbutton(Resultas ,text="Export",width=20,command=ExportRes,bg="cornflower blue")
    tkgrid(l1, row=0, column=0)
    tkgrid(Titre.1, row=1, column=1, sticky="w")
    tkconfigure(Titre.1, font=PoliceTitre)
    tkgrid(button1, row=1, column=2)

    tkgrid(l2, row=2, column=1)
    tkgrid(Titre.2, row=3, column=1, sticky="w")
    tkconfigure(Titre.2, font=PoliceTitre)
    tkgrid(button2, row=3, column=2)

    tkgrid(l3, row=4, column=1)
    tkgrid(Titre.3, row=5, column=1, sticky="w")
    tkconfigure(Titre.3, font=PoliceTitre)
    tkgrid(button3, row=5, column=2)
    tkgrid(button3b, row=5, column=3)

    tkgrid(l4, row=6, column=1)
    tkgrid(Titre.4, row=7, column=1, sticky="w")
    tkgrid(button4, row=7, column=2)
    tkconfigure(button4,padx=2)
    tkconfigure(Titre.4, font=PoliceTitre)
  }
  .include=function(...)
  {
    load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
    assign("Inc", tktoplevel(), envir = as.environment(pos))
    tkwm.geometry(Inc, "+550+270")

    assign("mm", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(mm)
    Titre <- tklabel(mm,text="Treated dose level",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DL <- tclVar(Res$MTD)
    for ( i in 1: Study$Nb_Dl)
    {
      tkgrid(assign(paste('rDL',i,sep=''),tkradiobutton(mm,variable=rbValue.DL ,value=i, text=paste(i),font=PoliceGenerale)))
    }

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Inc.mm1)
    Titre <- tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i),font=PoliceGenerale)))
    }
    tkgrid(assign(paste('rDLT',2,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=2, text=paste("pending"))))

    .Sauv=function(...)
    {
      assign("DL", c(Res$PT.data[,1],as.numeric(tclvalue((rbValue.DL)))), envir = as.environment(pos))
      assign("DLT", c(Res$PT.data[,2],as.numeric(tclvalue((rbValue.DLT)))), envir = as.environment(pos))
      assign("patid", c(Res$PT.data[,3],Res$Pat.included+1), envir = as.environment(pos))
      assign("pndg", as.numeric (c(Res$PT.data[,4],as.numeric(tclvalue((rbValue.DLT)))==2)), envir = as.environment(pos))

      Res$PT.data <- data.frame(DL=DL,DLT=DLT,patid=patid,pndg=pndg)
      Res$Pat.included <- Res$Pat.included+1

      DATA=Res$PT.data[which(Res$PT.data$pndg==0),]

      if (nrow(DATA)!=0)
      {
        if (sum(DATA[,2])==0)
        {
          print('3+3')
          numpat=length(DATA[,2])+1
          if (numpat!=1)
          {
            temp <- (numpat-1)/3
            if (temp==trunc(temp))
            {
              Res$MTD <- DATA$DL[numpat-1]+1
              if (Res$MTD>Study$Nb_Dl){Res$MTD <-DATA$DL[numpat-1]}}
            else
            {Res$MTD <-DATA$DL[numpat-1]}
          }
          save(Res,Study,file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
        }
        else
        {

          e$CRM.2 <- dfcrm::crm(prior=Study$DL, target=Study$Target, tox=DATA[,2],level=DATA[,1], conf.level = 0.95,
                                method = "mle", model =paste(Study$Model.n), intcpt=Study$Intcpt, scale=Study$Sd)
          Res$MTD <- e$CRM.2$mtd

          print('CRM')
          CRM.2 <- e$CRM.2
          save(Res,Study,CRM.2,file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
        }
      }

      load(file =paste(paste(names_Study,'crml',sep='-'),'RData',sep='.'))
      tkdestroy(Inc)
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }

      if(Res$Pat.included==1)
      {
        disabled.CRM()
        if (etatPrior==TRUE)
        {disabled.PriorCalib()}
        if (etatPrior==FALSE)
        {tkdestroy(PriorCal)}
      }


    }

    assign("Inc.Win_CRML", tkframe(Inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Inc.Win_CRML,text="Validate",width=30,command=.Sauv,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_CRML)

  }

  GrapheResults=function(...)
  {
    DATA=Res$PT.data[which(Res$PT.data$pndg==0),]
    n=length(DATA[,1])

    x=c(1:n)
    y=DATA[,1]
    z=DATA[,2]
    Data=data.frame(x=x,y=y,z=z)
    plot(x,y,pch=16,cex=2,col=(z+1),type='b',axes=FALSE,xlab="Patient number",ylab='Dose Level',main="Patients, Dose level and toxicity")
    axis(1, lwd = 2)
    axis(side=2,1:8, lwd = 2)
    legend("bottomright",inset = c(0.0, -0.19),  lwd=c(2.5,2.5),legend=c("Non Toxicity","Toxicity"), col = c(1,2), pch = 16,cex=0.8,xpd = NA)
  }

  Export.Graph=function()
  {
    GrapheResults()
  }

  curvesIC = function(...)
  {

    post.var <- e$CRM.2$post.var
    crit <- qnorm(0.5 + e$CRM.2$conf.level/2)
    doses=e$CRM.2$dosescaled
    Fempi=function(doses,beta){doses^(exp(beta))}
    Flogi=function(doses,beta){(1 + exp(-e$CRM.2$intcpt- exp(beta) * doses))^{-1}}
    Fmodel <- switch(e$CRM.2$model,empiric=Fempi, logistic=Flogi)
    plot(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),'l', lwd=2,xaxt="n",
         ylim=c(0, 1),xlab="Dose level", ylab="Proba. of DLT")
    axis(1, at = seq(1, length(e$CRM.2$prior), by = 1))
    abline(h=e$CRM.2$target,col="blue",lty=2,lwd=2)
    objtitle <-rbind("Dose-toxicity curve + 0.95_IC",paste(e$CRM.2$model, "model"))
    title(main=objtitle)
    legend('topright',c("Proba. of DLT", "IC(0.95)","target"),lty=c(1,1,2), col=c('black','red','blue'),lwd=2)
    points(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),lwd=2)
    if (post.var >= 0) {
      lb <- e$CRM.2$estimate - crit * sqrt(post.var)
      ub <- e$CRM.2$estimate + crit * sqrt(post.var)

      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,lb),col="red",lwd=2)
      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,ub),col="red",lwd=2)
    }else{print('Confidence interval not calculated')}
  }

  Export.CurvesIC=function()
  {
    if (!is.null(e$CRM.2)) {
      curvesIC()
    }else {print('Results are displayed once toxicity is observed')}
  }


  NewCRM=function()
  {
    tkdestroy(Win_CRML)
    frameInitCRM()
    PriorCalib()
  }

  disabled.CRM=function()
  {
    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save, row=27, column=0, sticky="se")

    Nb_Niv_Doses<- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding  ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Names of study:",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(labage, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,state="disabled",bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    .ReCap()
  }

  Open.CRM=function()
  {
    tkdestroy(Win_CRML)
    frameInitCRM()
    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save, row=27, column=0, sticky="se")

    Nb_Niv_Doses <- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")


    Tsd.<-tklabel(Saisi, text="Normal s.deviation",font=PoliceGenerale)
    n.iter <- tclVar(Study$Sd)
    sd.<-tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tsd., row=6,column=2, sticky="w")
    tkgrid(sd., row=6,column=3, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale)
    n.iter <- tclVar(Study$Intcpt)
    assign("intcpt.", tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    Titre=tklabel(Saisi,text="Names of study",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(Titre, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save Study",width=20,state="disabled")
    tkgrid(Save.rec, row=37, column=1, columnspan=2, rowspan=2)
    .ReCap()
    .ReCap.Resultat()
  }

  WordExport=function (dframe=NULL) {

    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}
    docx( ) %>%
      addFlexTable(dframe %>%
                     FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                               header.text.props = textBold(color = "white"),
                               add.rownames = TRUE ) %>%
                     setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
      writeDoc(file = "ResExport.docx")
    Save.word.infos <- tkmessageBox(title = "Word Export",
                                    message = "File ResExport.docx saved in the current working directory of the R proces", icon = "info", type = "ok")
  }

  ExcellExport=function (dframe=NULL) {
    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}

    filef_ <- tclvalue(tkgetSaveFile())
    if (filef_ != "") {
      filesave=paste(filef_,"xlsx",sep=".")

      wb_ <- createWorkbook()
      sheet1_ <- addWorksheet(wb_, sheetName="Simulation")

      style_ <- createStyle(fontSize = 12, fontColour = "black",
                            textDecoration = c("bold", "italic", "underline"),
                            halign = "center", valign = "center", border = "Bottom",
                            fgFill = "gray")

      setColWidths(wb_, sheet1_, cols=c(1,2,3,4,5), widths = 13.43)

      writeDataTable(wb_,sheet1_,x=dframe)

      addStyle(wb_,sheet1_,style_,cols=1:5,rows=1)

      saveWorkbook(wb_, filesave, overwrite = T)

    }
  }
  .RecapSim=function()
  {

    .Save_sim=function()
    {
      Study.file=function()
      {
        a00=paste(StudyS$Name)
        a0=paste('Name of study',paste(StudyS$Name),sep=':')
        a1=paste('Method','CRML',sep=':')
        a2='Continual Reassessment Method (CRM) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(StudyS$Name),'clsim', sep='.'))
      }
      Study.file()
      ResSim <- e$ResSim
      save(ResSim,StudyS,file =paste(paste(StudyS$Name,'clsim',sep='-'),'RData',sep='.'))
      Save.infos <- tkmessageBox(title = "Data storage Infos",
                                 message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

    assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.fr, row=0, column=0, rowspan=16)

    vect.X0=NULL
    for (i in 1:StudyS$Npat)
    {
      vect.X0[i]=as.numeric(tclvalue(tkget(eval(parse(text = paste('valx',i,sep=''))))))
    }

    StudyS$X0 = vect.X0
    if(is.unsorted(StudyS$X0)|(sum(is.na(StudyS$X0))==1)){stop(paste('Initial design must be a non-decreasing sequence of dose levels of length', StudyS$Npat))}
    if(any(StudyS$X0<1)|any(StudyS$X0>StudyS$Nb_Dl)|any(StudyS$X0%%1 != 0)) stop(paste('Initial design incorrect, enter a sequence of dose levels between 1 and',StudyS$Nb_Dl))

    assign("StudyS", StudyS, envir = as.environment(pos))

    cat("\n Submission in progress... Please wait... ", "\n")
    e$ResSim <- dfcrm::crmsim(PI=StudyS$True, prior=StudyS$Prior,
                              target=StudyS$Target,n=StudyS$Npat,x0=StudyS$X0,nsim=StudyS$Nsim,method="mle",
                              model=StudyS$Model,count=FALSE, intcpt=StudyS$Intcpt, scale=StudyS$Sd, seed=StudyS$Seed)


    PriorRes=paste(round(e$ResSim$prior[1],3))
    TrueRes=paste(round(e$ResSim$PI[1],3))
    MTDRes=paste(round(e$ResSim$MTD[1],3))
    ToxRes=paste(round(e$ResSim$tox[1],3))
    LevelRes=paste(round(e$ResSim$level[1],3))
    for( i in 2 : length(e$ResSim$prior))
    {
      PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
      TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
      MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
      LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
      ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
    }

    NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    Titre1<-tklabel(Rec.fr,text="Results of the simulation")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
    aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
    aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
    aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
    aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
    aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
    aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
    aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
    aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
    aff_1.1<-tklabel(Rec.fr,text=paste("Continued reassessment method "))
    aff_1.2<-tklabel(Rec.fr,text=paste("Maximum likelihood (CRML) "))
    aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
    aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
    aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
    aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
    aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
    aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
    aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
    aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
    aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
    aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
    aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
    aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

    tkgrid(aff_1.0, row=4, column=1,  sticky="w")
    tkgrid(aff_1.1, row=4, column=2,  sticky="w")
    tkgrid(aff_1.2, row=5, column=2,  sticky="w")
    tkconfigure(aff_1.0, font=Policeligne)
    tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
    tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

    tkgrid(aff_2.0, row=6, column=1, sticky="w")
    tkgrid(aff_2.1, row=6, column=2, sticky="w")
    tkconfigure(aff_2.0, font=Policeligne)
    tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

    tkgrid(aff_3.0, row=7, column=1, sticky="w")
    tkgrid(aff_3.1, row=7, column=2,  sticky="w")
    tkconfigure(aff_3.0, font=Policeligne )
    tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

    tkgrid(aff_4.0, row=8, column=1, sticky="w")
    tkgrid(aff_4.1, row=8, column=2,  sticky="w")
    tkconfigure(aff_4.0, font=Policeligne )
    tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

    tkgrid(aff_5.0, row=9, column=1,  sticky="w")
    tkgrid(aff_5.1, row=9, column=2,sticky="w")
    tkconfigure(aff_5.0, font=Policeligne )
    tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

    tkgrid(aff_6.0, row=10, column=1,  sticky="w")
    tkgrid(aff_6.1, row=10, column=2,  sticky="w")
    tkconfigure(aff_6.0, font=Policeligne)
    tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

    tkgrid(aff_7.0, row=11, column=1,  sticky="w")
    tkgrid(aff_7.1, row=11, column=2,  sticky="w")
    tkconfigure(aff_7.0, font=Policeligne )
    tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

    tkgrid(aff_11.0, row=12, column=1,  sticky="w")
    tkgrid(aff_11.1, row=12, column=2,  sticky="w")
    tkconfigure(aff_11.0, font=Policeligne )
    tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

    tkgrid(aff_8.0, row=13, column=1,  sticky="w")
    tkgrid(aff_8.1, row=13, column=2,  sticky="w")
    tkconfigure(aff_8.0, font=Policeligne )
    tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

    tkgrid(aff_9.0, row=14, column=1,  sticky="w")
    tkgrid(aff_9.1, row=14, column=2,  sticky="w")
    tkconfigure(aff_9.0, font=Policeligne )
    tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

    tkgrid(aff_10.0, row=15, column=1,  sticky="w")
    tkgrid(aff_10.1, row=15, column=2,  sticky="w")
    tkconfigure(aff_10.0, font=Policeligne )
    tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

    assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.save_, row=17, column=0)

    ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
    colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                         "Av. patient","Av. tox. ")
    WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                      command=function()ExcellExport(dframe=round(ResExp_,3)),bg="cornflower blue")
    tkgrid(WordExp_, row=1)
    blank<-tklabel(Rec.save_,text="      ")
    tkgrid(blank, row=2)

    SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,command=.Save_sim,bg="cornflower blue")
    tkgrid(SaveSim_, row=3)
    tk2notetab.select(net.CRML_, "Results")

  }

  disabled.SimCRM=function()
  {
    .RecapSimB=function()
    {

      PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
      PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

      assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=16)

      PriorRes=paste(round(e$ResSim$prior[1],3))
      TrueRes=paste(round(e$ResSim$PI[1],3))
      MTDRes=paste(round(e$ResSim$MTD[1],3))
      ToxRes=paste(round(e$ResSim$tox[1],3))
      LevelRes=paste(round(e$ResSim$level[1],3))
      for( i in 2 : length(e$ResSim$prior))
      {
        PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
        TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
        MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
        LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
        ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
      }


      NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
      tkgrid(NS, row=1, column=1,  sticky="w")
      tkconfigure(NS, font=PoliceEtude, foreground="white")

      Titre1<-tklabel(Rec.fr,text="Results of the simulation")
      tkgrid(Titre1, row=3, column=1)
      tkconfigure(Titre1, font=PoliceTitre)

      aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
      aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
      aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
      aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
      aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
      aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
      aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
      aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
      aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
      aff_1.1<-tklabel(Rec.fr,text=paste("Continued reassessment method "))
      aff_1.2<-tklabel(Rec.fr,text=paste("Maximum likelihood (CRML) "))
      aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
      aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
      aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
      aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
      aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
      aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
      aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
      aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
      aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
      aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
      aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
      aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

      tkgrid(aff_1.0, row=4, column=1,  sticky="w")
      tkgrid(aff_1.1, row=4, column=2,  sticky="w")
      tkgrid(aff_1.2, row=5, column=2,  sticky="w")
      tkconfigure(aff_1.0, font=Policeligne)
      tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
      tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

      tkgrid(aff_2.0, row=6, column=1, sticky="w")
      tkgrid(aff_2.1, row=6, column=2, sticky="w")
      tkconfigure(aff_2.0, font=Policeligne)
      tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

      tkgrid(aff_3.0, row=7, column=1, sticky="w")
      tkgrid(aff_3.1, row=7, column=2,  sticky="w")
      tkconfigure(aff_3.0, font=Policeligne )
      tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

      tkgrid(aff_4.0, row=8, column=1, sticky="w")
      tkgrid(aff_4.1, row=8, column=2,  sticky="w")
      tkconfigure(aff_4.0, font=Policeligne )
      tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

      tkgrid(aff_5.0, row=9, column=1,  sticky="w")
      tkgrid(aff_5.1, row=9, column=2,sticky="w")
      tkconfigure(aff_5.0, font=Policeligne )
      tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

      tkgrid(aff_6.0, row=10, column=1,  sticky="w")
      tkgrid(aff_6.1, row=10, column=2,  sticky="w")
      tkconfigure(aff_6.0, font=Policeligne)
      tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

      tkgrid(aff_7.0, row=11, column=1,  sticky="w")
      tkgrid(aff_7.1, row=11, column=2,  sticky="w")
      tkconfigure(aff_7.0, font=Policeligne )
      tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

      tkgrid(aff_11.0, row=12, column=1,  sticky="w")
      tkgrid(aff_11.1, row=12, column=2,  sticky="w")
      tkconfigure(aff_11.0, font=Policeligne )
      tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

      tkgrid(aff_8.0, row=13, column=1,  sticky="w")
      tkgrid(aff_8.1, row=13, column=2,  sticky="w")
      tkconfigure(aff_8.0, font=Policeligne )
      tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

      tkgrid(aff_9.0, row=14, column=1,  sticky="w")
      tkgrid(aff_9.1, row=14, column=2,  sticky="w")
      tkconfigure(aff_9.0, font=Policeligne )
      tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

      tkgrid(aff_10.0, row=15, column=1,  sticky="w")
      tkgrid(aff_10.1, row=15, column=2,  sticky="w")
      tkconfigure(aff_10.0, font=Policeligne )
      tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

      assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.save_, row=17, column=0)

      ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
      colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                           "Av. patient","Av. tox. ")
      WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                        command=function()ExcellExport(dframe=round(ResExp_,3)),bg="cornflower blue")
      tkgrid(WordExp_, row=1)
      blank<-tklabel(Rec.save_,text="      ")
      tkgrid(blank, row=2)

      SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,state="disabled",bg="cornflower blue")
      tkgrid(SaveSim_, row=3)

    }

    tkdestroy(Saisi_)
    tkdestroy(m2_)
    tkdestroy(Run_)
    tkdestroy(Saisi.design_)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)


    assign("Saisi.design_", tkframe(Design_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.design_, FALSE)
    tkgrid(Saisi.design_, row=1, column=1, columnspan=8, rowspan=40)

    Nb_Niv_Doses<- StudyS$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (StudyS$Prior[i])
      n.itert<- tclVar (StudyS$True[i])


      tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8, textvariable=n.itert,state="disabled"),envir = as.environment(pos)),row=r,column=1)

      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=3)

    }


    SliderValue_=tclVar(StudyS$Nb_Dl)
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Target)
    Target_<-tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")


    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Npat)
    n_<-tkentry(Saisi_, width=5, textvariable=n.iter ,state="disabled")
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Nsim)
    Nsim_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$X0[1])
    X0_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Seed)
    Seed_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_,state="disabled") ; rb2.1_ <- tkradiobutton(Saisi_,state="disabled")
    if (StudyS$Model == "logistic") {
      rbValue.M_ <- tclVar(1)
      n.intcpt <- tclVar(StudyS$Intcpt)}
    if (StudyS$Model == "empiric") {
      rbValue.M_ <- tclVar(2)
      n.intcpt <- tclVar("NA")}
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal s.deviation", font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Sd)
    sd_<-tkentry(Saisi_, width=7, textvariable=n.iter, state="disabled")
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept", font=PoliceGenerale)
    assign("intcpt_", tkentry(Saisi_, width=7, textvariable=n.intcpt, state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_, state="disabled") ;
    const2_ <- tkradiobutton(Saisi_, state="disabled")
    rbValue.const_ <- tclVar(as.numeric(StudyS$Constrain))
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=9, column=0, sticky="w")
    tkgrid(const1_, row=9,  column=1, sticky="w")
    tkgrid(const2_, row=10,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(StudyS$Name)
    names.S_<-tkentry(Saisi_, width=20,textvariable=Nam_,state="disabled")
    tkgrid(labage_, row=11,column=0, sticky="w")
    tkgrid(names.S_, row=11,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,state="disabled",bg="cornflower blue")
    tkgrid(RunSim_, row=1)

    Nb.x0<-StudyS$Npat

    for ( i in 1: Nb.x0)
    {

      n.iter=tclVar(StudyS$X0[i])
      if (i <= 15) {
        tkgrid(assign(paste('labx',i,sep=''),tklabel(Saisi.design_, text=paste('Dose level P',i,sep=''),font=PoliceGenerale)),row=i,column=0)
        tkgrid(assign(paste('valx',i,sep=''),tkentry(Saisi.design_, width=8,textvariable=n.iter, state="disabled"),envir = as.environment(pos)),row=i,column=1)
      }
      if ((i > 15)&&(i <= 30)) {
        tkgrid(assign(paste('labx',i,sep=''),tklabel(Saisi.design_, text=paste('Dose level P',i,sep=''),font=PoliceGenerale)),row=i-15,column=3)
        tkgrid(assign(paste('valx',i,sep=''),tkentry(Saisi.design_, width=8,textvariable=n.iter, state="disabled"),envir = as.environment(pos)),row=i-15,column=4)
      }
      if (i > 30) {
        tkgrid(assign(paste('labx',i,sep=''),tklabel(Saisi.design_, text=paste('Dose level P',i,sep=''),font=PoliceGenerale)),row=i-30,column=6)
        tkgrid(assign(paste('valx',i,sep=''),tkentry(Saisi.design_, width=8,textvariable=n.iter, state="disabled"),envir = as.environment(pos)),row=i-30,column=7)
      }
    }
    if (Nb.x0 <= 15) {
      ValInitD_=tkbutton(Saisi.design_ ,text="Validation Init Design",width=20,
                         state="disabled",bg="cornflower blue")
      tkgrid(ValInitD_, row=16, column=1)}


    if ((Nb.x0 > 15)&&(Nb.x0 <= 30)) {
      ValInitD_=tkbutton(Saisi.design_ ,text="Validation Init Design",width=20,
                         state="disabled",bg="cornflower blue")
      tkgrid(ValInitD_, row=16, column=4)
    }

    if (Nb.x0 > 30) {
      ValInitD_=tkbutton(Saisi.design_ ,text="Validation Init Design",width=20,
                         state="disabled",bg="cornflower blue")
      tkgrid(ValInitD_, row=16,column=7)
    }

    .RecapSimB()
  }
  SimCRM=function()
  {

    tkdestroy(Win_CRML)
    frameInitSIM()

    .Crea.f_=function(...)
    {
      Nb_Niv_Doses_<-as.numeric(tclvalue((SliderValue_)))
      assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)

      for ( i in 1: Nb_Niv_Doses_)
      {
        r=17 + i
        tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=1)

        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=3)
      }
      if( Nb_Niv_Doses_!=8)
      {
        for ( i in (Nb_Niv_Doses_+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)

          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=2)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=3)
        }
      }
    }

    .Param.c_= function(...)
    {
      if (as.numeric(tclvalue((rbValue.M_)))==2)
      {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt_", tkentry(Saisi_, width=7, state="disabled"), envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")

      }
      else {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
        assign("intcpt_", tkentry(Saisi_, width=7), envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")
      }
    }


    .Simulation_=function (...)
    {
      .Sauvgarde.Study_=function(...)
      {
        Nb_Dl=as.numeric(tclvalue(SliderValue_))
        prior=NULL
        true=NULL
        if (Nb_Dl==2) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))))}
        if (Nb_Dl==3) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))))}
        if (Nb_Dl==4) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))))}
        if (Nb_Dl==5) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))))}
        if (Nb_Dl==6) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))))}
        if (Nb_Dl==7) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))))}
        if (Nb_Dl==8) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))),as.numeric(tclvalue(tkget(valt8))))}


        seed=1
        target=as.numeric(tclvalue(tkget((Target_))))

        Model=as.numeric(tclvalue((rbValue.M_)))

        npat=as.numeric(tclvalue(tkget(n_)))
        nsim=as.numeric(tclvalue(tkget((Nsim_))))
        Const=as.logical(as.numeric(tclvalue((rbValue.const_))))
        sdv=as.numeric(tclvalue(tkget((sd_))))

        seed=as.numeric(tclvalue(tkget((Seed_))))
        assign("Names_Study", tclvalue(tkget(names.S_)), envir = as.environment(pos))
        if (Model == 1) {model=as.character("logistic"); intcp=as.numeric(tclvalue(tkget((intcpt_))))}
        if (Model == 2) {model=as.character("empiric"); intcp=NULL}
        res.users <- 1
        if (target>0.55) cat("\n Warning: Target DLT rate too high")
        if (target<0.15) cat("\n Warning: Target DLT rate too low")
        if (npat <=0) stop('Number of patients to be enrolled <=0')
        if (npat > 100) cat("\n Warning: Number of patients to be enrolled > 100")
        if (npat > 80) cat("\n TIP: Might need to maximize the window to validate initial design")
        if (nsim < 100) cat("\n Warning: Low number of simulations")
        check_nsim <- function (){
          res.users <- 2
          while (res.users != 0 & res.users != 1) {
            cat("\n Warning: Large number of simulations, continue?  y or n")
            yn <- readLines(n=1)
            if(yn == "y" | yn == "Y" | yn== "yes" | yn == "Yes"){res.users <- 1;}
            if(yn == "n" | yn == "N" | yn== "no" | yn == "No"){res.users <- 0;}
          }
          return(res.users)
        }

        if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
        if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
        if (any(true <= 0) | any(true >= 1)) stop('True probabilities must be within ]0,1[ ')
        if (is.unsorted(true, strictly = TRUE)) stop('True probabilities should be monotonically increasing')
        if (nsim > 10000)  {res.users <- check_nsim()}

        if (res.users == 1) {
          tkdestroy(Saisi.design_)

          assign("Saisi.design_", tkframe(Design_,relief="groove",borderwidth=3), envir = as.environment(pos))
          tkpack.propagate(Saisi.design_, FALSE)
          tkgrid(Saisi.design_, row=1, column=1, columnspan=8, rowspan=40)

          v.x0 <- dfcrm::getinit(prior=prior, target=target, nK=ifelse(npat < Nb_Dl*2, npat,round(npat/Nb_Dl)),n=npat, method = "mle", detail = FALSE)
          for ( i in 1: npat)
          {
            ind <- floor(i/20.01)
            n.iter=tclVar(round(v.x0[i],3))
            if ((i > ind*20)&&(i <= (ind+1)*20)) {
              tkgrid(assign(paste('labx',i,sep=''),tklabel(Saisi.design_, text=paste('Dose level P',i,sep=''),font=PoliceGenerale)),row=i-ind*20,column=3*ind)
              tkgrid(assign(paste('valx',i,sep=''),tkentry(Saisi.design_, width=8,textvariable=n.iter),envir = as.environment(pos)),row=i-ind*20,column=3*ind+1)
            }

          }

          ValInitD_=tkbutton(Saisi.design_ ,text="Validation Init Design",width=20,
                             command=.RecapSim,bg="cornflower blue")
          tkgrid(ValInitD_, row=21, column=3*ind+1)

          tk2notetab.select(net.CRML_, "Initial design")

          assign("StudyS", list(Name=paste(Names_Study), Nb_Dl=Nb_Dl, True=true, Prior=prior, Target=target,
                                Model=model, Npat=npat,Nsim=nsim, X0=v.x0, Intcpt=intcp, Sd=sdv,
                                Constrain=Const, Seed=seed), envir = as.environment(pos))


        }
      }
      .Sauvgarde.Study_()

    }


    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")


    SliderValue_=tclVar('8')

    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",command=.Crea.f_)

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    assign("Target_", tkentry(Saisi_, width=5), envir = as.environment(pos))

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    l<-tklabel(Saisi_,text="            ")
    tkgrid(l, row=2, column=3)

    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    assign("n_", tkentry(Saisi_, width=5), envir = as.environment(pos))
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    Nsim_<-tkentry(Saisi_, width=10)
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    Seed_<-tkentry(Saisi_, width=10)
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_,command=.Param.c_) ; rb2.1_ <- tkradiobutton(Saisi_,command=.Param.c_)
    rbValue.M_ <- tclVar(1)
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal s.deviation",font=PoliceGenerale)
    sd_<-tkentry(Saisi_, width=7)
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
    assign("intcpt_", tkentry(Saisi_, width=7), envir = as.environment(pos))
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_) ; const2_ <- tkradiobutton(Saisi_)
    rbValue.const_ <- tclVar(1)
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=9, column=0, sticky="w")
    tkgrid(const1_, row=9,  column=1, sticky="w")
    tkgrid(const2_, row=10,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    names.S_<-tkentry(Saisi_, width=20)
    tkgrid(labage_, row=11,column=0, sticky="w")
    tkgrid(names.S_, row=11,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,command=.Simulation_,bg="cornflower blue")
    tkgrid(RunSim_, row=1)

  }

  save_Qf2=function(){ tkmessageBox(title = "Data storage Infos",
                                    message = "Data are automatically saved after each inclusion", icon = "info", type = "ok")}
  save_Qf=function(){ tkmessageBox(title = "Data storage Infos",
                                   message = "Please use the button save study available in interactive CRM or simulator to save your data", icon = "info", type = "ok")}
  about=function(){ tkmessageBox(title = "Information",
                                 message = "Continual Reassessment Method with Maximum
                                 likelihood estimation Interface 2017",
                                 icon = "info", type = "ok")}

  Open.help=function() {browseURL("https://cran.r-project.org/package=dfcrm", browser=getOption("browser"),
                                  encodeIfNeeded = FALSE) }

  frameInitCRM=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))

    assign("Win_CRML", tktoplevel(background = "black"), envir = as.environment(pos))
    tkwm.geometry(Win_CRML, "680x555")
    tkwm.geometry(Win_CRML, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRML, ilogo)
    tktitle(Win_CRML) <- "GUI CRML"
    tkpack.propagate(Win_CRML, FALSE)

    topMenu <- tk2menu(Win_CRML)
    tkconfigure(Win_CRML, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function (){tkdestroy(Win_CRML);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf2)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRML))

    assign("net.CRML", tk2notebook(Win_CRML, tabs = c("Prior calibration","Input parameters","Include","Results")),
           envir = as.environment(pos))
    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.CRML , fill = "both", expand = 1)

    assign("Include", tk2notetab(net.CRML , "Include"), envir = as.environment(pos))
    tkpack.propagate(Include, FALSE)

    assign("Resultas", tk2notetab(net.CRML , "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas, FALSE)

    assign("Required", tk2notetab(net.CRML , "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required, FALSE)

    assign("PriorCal", tk2notetab(net.CRML , "Prior calibration"), envir = as.environment(pos))
    tkpack.propagate(PriorCal, FALSE)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Rec.Win_CRML", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.Win_CRML, row=9, column=2, rowspan=16)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, columnspan=3, rowspan=16)

    assign("halfwidthPrior", NULL, envir = as.environment(pos))
    assign("nuPrior", NULL, envir = as.environment(pos))
    assign("etatPrior", FALSE, envir = as.environment(pos))
  }

  frameInitSIM=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))

    assign("Win_CRML", tktoplevel(background = "black"), envir = as.environment(pos))
    tkwm.geometry(Win_CRML, "680x585")
    tkwm.geometry(Win_CRML, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRML, ilogo)
    tktitle(Win_CRML) <- "GUI CRML"
    tkpack.propagate(Win_CRML, FALSE)

    topMenu <- tk2menu(Win_CRML)
    tkconfigure(Win_CRML, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function (){tkdestroy(Win_CRML);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRML))

    assign("net.CRML_", tk2notebook(Win_CRML, tabs = c("Input parameters","Initial design","Results")), envir = as.environment(pos))
    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.CRML_ , fill = "both", expand = 1)

    assign("Resultas_", tk2notetab(net.CRML_ , "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas_, FALSE)

    assign("Design_", tk2notetab(net.CRML_ , "Initial design"), envir = as.environment(pos))
    tkpack.propagate(Design_, FALSE)

    assign("Required_", tk2notetab(net.CRML_ , "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required_, FALSE)

    assign("Saisi.design_", tkframe(Design_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.design_, FALSE)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)


    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkpack.propagate(m2_, FALSE)
  }

  frameInit=function(){
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))

    assign("Win_CRML", tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_CRML, "630x472")
    tkwm.geometry(Win_CRML, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_CRML, ilogo)
    tktitle(Win_CRML) <- "GUI CRML"
    tkpack.propagate(Win_CRML, FALSE)
    topMenu <- tk2menu(Win_CRML)
    tkconfigure(Win_CRML, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study", command=function (){tkdestroy(Win_CRML);frameInit()})
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_CRML))

  }
  frameInit()
}

.TITECRM=function(pos=1, e)
{
  e$ResSim = NULL
  e$CRM.2 = NULL
  tclRequire("Tktable")
  assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12), envir = as.environment(pos))
  assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10), envir = as.environment(pos))

  remplireRequid=function()
  {

    .Sauvgarde.Study=function(...)
    {
      Nb_Dl=as.numeric(tclvalue(SliderValue))
      DL=NULL
      if (Nb_Dl==2) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))}
      if (Nb_Dl==3) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))}
      if (Nb_Dl==4) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))}
      if (Nb_Dl==5) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))}
      if (Nb_Dl==6) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))}
      if (Nb_Dl==7) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))}
      if (Nb_Dl==8) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))}

      Target=as.numeric(tclvalue(tkget((Target))))

      Model=as.numeric(tclvalue((rbValue.M)))

      if (Model==1){Model.n="logistic"; intcpt=as.numeric(tclvalue(tkget(intcpt.)))}
      if (Model==2){Model.n="empiric"; intcpt=NULL}

      Sdv=as.numeric(tclvalue(tkget(sd.)))
      Stop=as.numeric(tclvalue(rbValue.SC))

      N_Pat=as.numeric(tclvalue(tkget(NBpat)))

      N_pat_Level=as.numeric(tclvalue(tkget(NBpat)))
      ObWin=as.numeric(tclvalue(tkget(ObWin)))

      if ((Stop == 1) & (N_Pat <=0)) stop('Number of patients to be enrolled in the study <=0')
      if ((Stop == 1) & (N_Pat > 100)) cat("\n Warning: Number of patients to be enrolled in the study > 100")
      if ((Stop == 2) & (N_Pat <=0)) stop('Number of patients by dose level  <=0')
      if ((Stop == 2) & (N_Pat > 20)) cat("\n Warning: Large number of patients by dose level")
      if (ObWin <= 0 ) stop('Observation window incorrect')
      assign("names_Study", tclvalue(tkget(names.S)), envir = as.environment(pos))

      assign("Study", list(name=paste(names_Study),Nb_Dl=Nb_Dl,DL=DL,Target=Target,Model=Model,
                           Model.n=Model.n,Stop=Stop,N_Pat=N_Pat,N_pat_Level=N_pat_Level,Obs.wind=ObWin,
                           halfwidthPrior=halfwidthPrior,nuPrior=nuPrior, Sd=Sdv, Intcpt=intcpt), envir = as.environment(pos))

      differ=abs(Study$DL-Study$Target)
      MTD=1
      for ( i in 1 : Study$Nb_Dl)
      {
        if (differ[i]==min(differ)) {MTD=i}
      }

      Pat_include=0
      PT.data=NULL

      assign("Res", list(MTD=MTD,Pat.included=Pat_include,PT.data=PT.data,ObWin=ObWin), envir = as.environment(pos))
      save(Res,Study,file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'))
      .ReCap()

      Study.file=function()
      {
        a00=paste(names_Study)
        a0=paste('Name of study',paste(names_Study),sep=':')
        a1=paste('Method','TiTE_CRM',sep=':')
        a2='TiME TO EVENTS-Continual Reassessment Method (CRM) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(names_Study),'tcrm', sep='.'))
      }
      Study.file()
      tk2notetab.select(net.TiTECRM , "Include")
    }

    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)
    tk2notetab.select(net.TiTECRM , "Input parameters")


    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=2, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")
    priorProbaf <- c()

    Nb_Niv_Doses <- nlevel
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (tkget(eval(parse(text = paste('valp',i,sep='')))))
      priorProbaf <- c(priorProbaf,as.numeric(tclvalue(n.iter)))
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    if (any(priorProbaf <= 0) | any(priorProbaf >= 1)) stop('Prior probabilities must be within ]0,1[ ')
    if (is.unsorted(priorProbaf, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')

    SliderValue=tclVar(nlevel)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(TargetPrior)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter,state="disabled")
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi) ; rb2 <- tkradiobutton(Saisi)
    rbValue.SC <- tclVar(1)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NBpat<-tkentry(Saisi, width=10)
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi, state="disabled") ; rb2.1 <- tkradiobutton(Saisi, state="disabled")
    rbValue.M <- tclVar(modelPrior)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Tsd.<-tklabel(Saisi, text="Normal S. deviation",font=PoliceGenerale)
    n.iter <- tclVar(sdPrior)
    sd.<-tkentry(Saisi,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=8,column=0, sticky="w")
    tkgrid(sd., row=8,column=1, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(intcptPrior)}
    else {tclVar("NA")}
    assign("intcpt.", tkentry(Saisi, textvariable=n.iter, width=10, state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt., row=8,column=2, sticky="w")
    tkgrid(intcpt., row=8,column=3, sticky="w")

    labagewin<-tklabel(Saisi, text="Observation window ",font=PoliceGenerale)
    ObWin<-tkentry(Saisi, width=10)
    tkgrid(labagewin, row=9,column=0, sticky="w")
    tkgrid(ObWin, row=9,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    Nam<- tclVar()
    names.S<-tkentry(Saisi, width=20)

    tkgrid(labage, row=10,column=0, sticky="w")
    tkgrid(names.S, row=10,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,command=.Sauvgarde.Study,bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    assign("etatPrior", TRUE, envir = as.environment(pos))
  }

  disabled.PriorCalib=function(...)
  {
    if(is.tkwin(affCAL)){tkdestroy(affCAL)}
    if(is.tkwin(SaisiCal)){tkdestroy(SaisiCal)}
    if(is.tkwin(Saisi.saveCal )){tkdestroy(Saisi.saveCal )}


    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)

    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)

    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.saveCal, FALSE)
    SliderValue.CAL=tclVar(Study$Nb_Dl)
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz", state="disabled")
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)

    for ( i in 1: Study$Nb_Dl)
    {
      r=10 + i
      assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
      n.iter=tclVar (tkget(eval(parse(text = paste('val',i,sep='')))))
      tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL,text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL,state="disabled", width=8,textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=1)
    }

    if( Study$Nb_Dl!=8)
    {
      for ( i in (Study$Nb_Dl+1):8)
      {
        r=10 + i
        tkgrid(assign(paste('lab',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
        tkgrid(assign(paste('val',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
      }
    }

    Titre=tklabel(SaisiCal,text="Target DLT rate ",font=PoliceGenerale)
    n.iter=tclVar(round(TargetPrior,3))
    Target.cal<-tkentry(SaisiCal,textvariable=n.iter,state="disabled", width=5)

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    n.iter=tclVar(halfwidthPrior)
    halfwidth<-tkentry(SaisiCal, width=10,textvariable=n.iter,state="disabled")
    tkgrid(lab_halfwidth, row=3,column=0,columnspan=2, sticky="w")
    tkgrid(halfwidth, row=3,column=2, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal,state="disabled") ; rb2.1CAL <- tkradiobutton(SaisiCal,state="disabled")
    assign("rbValue.MCAL", tclVar(modelPrior), envir = as.environment(pos))
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    n.iter=tclVar(nuPrior)
    nu<-tkentry(SaisiCal, width=10, textvariable=n.iter, state="disabled")
    tkgrid(lab_nu, row=6,column=0, sticky="w")
    tkgrid(nu, row=6,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="NormalS. deviation  ",font=PoliceGenerale)
    n.iter=tclVar(Study$Sd)
    sd.<-tkentry(SaisiCal,textvariable=n.iter, width=10, state="disabled")
    tkgrid(Tsd., row=7,column=0, sticky="w")
    tkgrid(sd., row=7,column=1, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    n.iter <- if (modelPrior == 1) {tclVar(Study$Intcpt)}
    else {tclVar("NA")}
    assign("intcpt.", tkentry(SaisiCal, textvariable=n.iter, width=10, state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,bg = "cornflower blue",state="disabled")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,bg="cornflower blue",state="disabled")
    tkgrid(Validation, row=2)
  }

  PriorCalib=function(...)
  {
    .Crea.f.CAL=function(...)
    {
      assign("TargetPrior", as.numeric(tclvalue(tkget((Target.cal)))), envir = as.environment(pos))
      assign("halfwidthPrior", as.numeric(tclvalue(tkget((halfwidth)))), envir = as.environment(pos))
      assign("nuPrior", as.numeric(tclvalue(tkget(nu))), envir = as.environment(pos))
      assign("nlevel", as.numeric(tclvalue(tkget((slider_Nb_DL.CAL)))), envir = as.environment(pos))
      assign("modelPrior", as.numeric(tclvalue(((rbValue.MCAL)))), envir = as.environment(pos))
      assign("sdPrior", as.numeric(tclvalue(tkget((sd.)))), envir = as.environment(pos))
      assign("intcptPrior", as.numeric(tclvalue(tkget((intcpt.)))), envir = as.environment(pos))

      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))
      assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)

      if (TargetPrior>0.55) cat("\n Warning: Target DLT rate too high")
      if (TargetPrior<0.15) cat("\n Warning: Target DLT rate too low")
      if ((halfwidthPrior<=0)|(halfwidthPrior>0.5)) stop('Halfwidth of the indifference intervals incorrect!')
      if ((halfwidthPrior<=0.5)&(halfwidthPrior>0.2)) cat("\n Warning: Try to reduce the value of the halfwidth of the indifference intervals ")
      if ((nuPrior<1)|(nuPrior>Nb_Niv_DosesCAL)|(nuPrior%%1 != 0)) stop(paste('Prior guess of MTD incorrect, enter an integer between 1 and',Nb_Niv_DosesCAL))

      if (modelPrior==1)
      {
        e$prior <- dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior, nlevel,
                                   model="logistic", intcpt=intcptPrior)
      }

      if (modelPrior==2)
      {
        e$prior <- dfcrm::getprior(halfwidthPrior, TargetPrior, nuPrior,
                                   nlevel, model="empiric")
      }


      for ( i in c(1:Nb_Niv_DosesCAL))
      {
        r=10 + i
        assign(paste('prior',i,sep='_'),round(e$prior[i],3),envir = as.environment(pos))
        n.iter=tclVar(round(e$prior[i],3))
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,textvariable=n.iter),envir = as.environment(pos)),row=r,column=1)
      }

      if( Nb_Niv_DosesCAL!=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i
          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }
    }

    f.CALPrior=function(...)
    {
      if(is.tkwin(affCAL)){tkdestroy(affCAL)}
      assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(affCAL, row=10)
      assign("Nb_Niv_DosesCAL", as.numeric(tclvalue((SliderValue.CAL))), envir = as.environment(pos))

      for ( i in 1: Nb_Niv_DosesCAL)
      {
        r=10 + i
        tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8),envir = as.environment(pos)),row=r,column=1)
      }

      if(Nb_Niv_DosesCAL !=8)
      {
        for ( i in (Nb_Niv_DosesCAL+1):8)
        {
          r=10 + i
          tkgrid(assign(paste('labp',i,sep=''),tklabel(affCAL, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(affCAL, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }

    }
    .Param.choice = function(...)
    {
      if (as.numeric(tclvalue((rbValue.MCAL)))==2)
      {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt.", tkentry(SaisiCal, width=10, state="disabled"), envir = as.environment(pos))
        tkgrid(Tintcpt., row=6,column=2, sticky="w")
        tkgrid(intcpt., row=6,column=3, sticky="w")
      }
      else {
        Tintcpt.<-tklabel(SaisiCal, text="intercept",font=PoliceGenerale)
        assign("intcpt.", tkentry(SaisiCal, width=10), envir = as.environment(pos))
        tkgrid(Tintcpt., row=6,column=2, sticky="w")
        tkgrid(intcpt., row=6,column=3, sticky="w")
      }
    }

    assign("affCAL", tkframe(PriorCal,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(affCAL, row=10)

    assign("SaisiCal", tkframe(PriorCal,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(SaisiCal, FALSE)
    tkgrid(SaisiCal, row=3)
    assign("Saisi.saveCal", tkframe(PriorCal,borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.saveCal, FALSE)
    SliderValue.CAL=tclVar('8')
    slider_Nb_DL.CAL <- tkscale(SaisiCal, from=2, to=8,showvalue=T, variable=SliderValue.CAL, resolution=1, orient="horiz",command=f.CALPrior)
    tkgrid(tklabel(SaisiCal,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL.CAL , row=1,column=1, sticky="w")

    Titre=tklabel(SaisiCal,text="Target DLT rate ",font=PoliceGenerale)
    Target.cal<-tkentry(SaisiCal, width=5)

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target.cal, row=2, column=1, sticky="w")

    lab_halfwidth<-tklabel(SaisiCal, text="The desired halfwidth of the indifference intervals",font=PoliceGenerale)
    halfwidth<-tkentry(SaisiCal, width=10)
    tkgrid(lab_halfwidth, row=3,column=0, sticky="w", columnspan=2)
    tkgrid(halfwidth, row=3,column=2, sticky="w")

    Titre=tklabel(SaisiCal,text="Model",font=PoliceGenerale)
    rb1.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice) ;
    rb2.1CAL <- tkradiobutton(SaisiCal, command=.Param.choice)
    assign("rbValue.MCAL", tclVar(1), envir = as.environment(pos))
    tkconfigure(rb1.1CAL,variable=rbValue.MCAL,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1CAL,variable=rbValue.MCAL,value=2, text="Empiric",font=PoliceGenerale)
    tkgrid(Titre, row=4,column=0, sticky="w")
    tkgrid(rb1.1CAL, row=4,column=1, sticky="w")
    tkgrid(rb2.1CAL, row=5,column=1, sticky="w")

    Tsd.<-tklabel(SaisiCal, text="Normal S. deviation ",font=PoliceGenerale)
    sd.<-tkentry(SaisiCal, width=10)
    tkgrid(Tsd., row=6,column=0, sticky="w")
    tkgrid(sd., row=6,column=1, sticky="w")

    Tintcpt.<-tklabel(SaisiCal, text="Intercept",font=PoliceGenerale)
    assign("intcpt.", tkentry(SaisiCal, width=10), envir = as.environment(pos))
    tkgrid(Tintcpt., row=6,column=2, sticky="w")
    tkgrid(intcpt., row=6,column=3, sticky="w")

    lab_nu<-tklabel(SaisiCal, text="The prior guess of MTD",font=PoliceGenerale)
    nu<-tkentry(SaisiCal, width=10)
    tkgrid(lab_nu, row=7,column=0, sticky="w")
    tkgrid(nu, row=7,column=1, sticky="w")

    p<-tklabel(SaisiCal, text=" ",font=PoliceGenerale)
    tkgrid(p, row=8)
    getprio=tkbutton(SaisiCal ,text="Run",width=20,command=.Crea.f.CAL,bg = "cornflower blue")
    tkgrid(getprio , row=9, column=3)

    tkgrid(Saisi.saveCal,column=0,rowspan=2 ,sticky="e")
    Validation=tkbutton(Saisi.saveCal ,text="Validation of prior",width=20,command=remplireRequid,bg="cornflower blue")
    tkgrid(Validation, row=2)
  }

  loads <- function()
  {
    loads.infos <- tkmessageBox(title = "Data loading Infos",
                                message = "Select a file.tcrm or file.tcsim", icon = "info", type = "ok")

    filef <- tclvalue(tkgetOpenFile())
    stu=read.table(filef,header=TRUE)
    assign("names_Study", paste(stu[1,]), envir = as.environment(pos))
    extension<-str_sub(filef,-4)


    if (extension == "tcrm") {
      load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'),envir = as.environment(pos))

      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      e$CRM.2 <- CRM.2
      Open.CRM()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          tkdestroy(Include)
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            tkdestroy(Include)
          }
        }
      }
    }

    if (extension == "csim") {
      load(file =paste(paste(names_Study,'tcsim',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      e$ResSim <- ResSim
      assign("StudyS", StudyS, envir = as.environment(pos))
      tkdestroy(Win_TiTECRM)
      frameInitSIM()
      disabled.SimCRM()

    }
  }

  .ReCap=function(...)
  {
    load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))


    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("TiTE-Continued reassessment method-bayesian approach (TiTECRM) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model==1,"Logistic","Empiric")))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_TiTECRM", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    Save=tkbutton(Rec.Win_TiTECRM,text="New patient",width=30,command=.Include,bg="cornflower blue")
    Modif=tkbutton(Rec.Win_TiTECRM,text="Modif patient data",width=30,command=.modif.data.patient,bg="cornflower blue")
    tkgrid(Rec.Win_TiTECRM, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    tkgrid(Modif, row=0, column=1)
  }

  .modif.data.patient= function (...)
  {
    .modif.data=function (...)
    {

      assign("id.modif", as.numeric(tkcurselection(tlm)) + 1, envir = as.environment(pos))
      tkdestroy(tt.modif)
      m.data=Res$PT.data[id.modif,]
      win.m <- tktoplevel()
      dlt.      <- tclVar(m.data[,2])
      level.    <- tclVar(m.data[,1])
      followup. <- tclVar(m.data[,4])
      lab.id. <- tklabel(win.m,text=paste("Patient N_",id.modif),font=PoliceGenerale)
      tkgrid(lab.id., row=0, columnspan=2)
      lab.level. <- tklabel(win.m,text=paste("Dose level"),font=PoliceGenerale)
      tkgrid(lab.level., row=1, columnspan=2)
      assign("win.level", tk2entry(win.m, width = "3", textvariable = level.), envir = as.environment(pos))
      tkgrid(win.level,row=1,column=2)
      lab.dlt. <- tklabel(win.m,text=paste("DLT (1= 'Yes', 0='No')"),font=PoliceGenerale)
      tkgrid(lab.dlt., row=3, columnspan=2)
      assign("win.dlt", tk2entry(win.m, width = "3", textvariable = dlt.), envir = as.environment(pos))
      tkgrid(win.dlt,row=3,column=2)
      lab.fup. <- tklabel(win.m,text=paste("follow-up","<=",Res$ObWin),font=PoliceGenerale)
      tkgrid(lab.fup., row=5, columnspan=2)
      assign("win.fup", tk2entry(win.m, width = "3", textvariable = followup.), envir = as.environment(pos))
      tkgrid(win.fup,row=5,column=2)


      onOK <- function() {
        new.dlt.  <- as.numeric(tclvalue(tkget(win.dlt)))
        new.level <- as.numeric(tclvalue(tkget(win.level)))
        new.fup   <- as.numeric(tclvalue(tkget(win.fup)))
        Res$PT.data[id.modif,c(2,1,4)] <- c(new.dlt.,new.level,new.fup)
        e$CRM.2 <- dfcrm::titecrm(prior=Study$DL, target=Study$Target, tox=Res$PT.data[,2],level=Res$PT.data[,1], conf.level = 0.95,
                                  model = paste(Study$Model.n), followup=Res$PT.data[,4], obswin=Res$ObWin, intcpt=Study$Intcpt,
                                  scale=Study$Sd)
        Res$MTD <- e$CRM.2$mtd
        CRM.2 <- e$CRM.2
        save(Res,Study,CRM.2,file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'))
        load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'),envir = as.environment(pos))
        tkdestroy(win.m)
        assign("Res", Res, envir = as.environment(pos))
        assign("Study", Study, envir = as.environment(pos))

        .ReCap()
        .ReCap.Resultat()
      }
      win.OK <-tkbutton(win.m, text = "OK", width = 10, command = onOK, bg="cornflower blue")
      tkgrid(win.OK)
    }

    m.data=Res$PT.data
    if (is.null(m.data))
    {
      msg <- paste("No patient included",sep="")
      tkmessageBox(message=msg)
    }
    if (!is.null(m.data))
    {
      if (nrow(m.data)>=1)
      {
        tt.modif=tktoplevel()
        tkwm.geometry(tt.modif, "+650+400")
        assign("tlm", tklistbox(tt.modif,height=nrow(m.data)+1,selectmode="single",background="white"),
               envir = as.environment(pos))
        tkgrid(tklabel(tt.modif,text="Select a patient",font=PoliceGenerale))
        tkgrid(tlm)
        patlist=paste(" Patient N_", as.character(m.data$patid),sep="")
        for (i in (1:length(patlist)))
        {
          tkinsert(tlm,"end",patlist[i])
        }
        tkselection.set(tlm,0)
        OK.but <-tkbutton(tt.modif,text="  OK  ",command=.modif.data,bg="cornflower blue")
        tkgrid(OK.but)
      }
    }
  }
  disabled.ReCap=function(...)
  {
    tkdestroy(Rec.mm0)
    tkdestroy(Rec.Win_TiTECRM)

    load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("TiTE-Continued reassessment method-bayesian approach (TiTECRM) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model==1,"Logistic","Empiric")))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")


    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"),font=PoliceGenerale)
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included),font=PoliceGenerale)

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"),font=PoliceGenerale)
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD),font=PoliceGenerale)

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    DATA=Res$PT.data[which(Res$PT.data$pndg==1),]
    if (nrow(DATA)==0)
    {
      print('CAS 1')
      assign("Rec.Win_TiTECRM", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_TiTECRM,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Modif=tkbutton(Rec.Win_TiTECRM,text="Modif patient data",width=30,state="disabled",bg="cornflower blue")
      tk2notetab.select(net.TiTECRM , "Resultas")
      print(' Fin CAS 1')

      tkgrid(Rec.Win_TiTECRM, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Modif, row=0, column=1)

    }
    if (nrow(DATA)!=0)
    {
      print('CAS 2')
      assign("Rec.Win_TiTECRM", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
      Save=tkbutton(Rec.Win_TiTECRM,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Modif=tkbutton(Rec.Win_TiTECRM,text="Modif patient data",width=30,state="disabled",bg="cornflower blue")
      tkgrid(Rec.Win_TiTECRM, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Modif, row=0, column=1)
      print(' Fin CAS 2')
    }
  }


  .ReCap.Resultat=function(...)
  {


    Resultsbypatient=function()
    {
      assign("myRarray1", data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2],
                                     followup=Res$PT.data[,4], stringsAsFactors=FALSE), envir = as.environment(pos))

      X=colnames(myRarray1)
      myRarray=rbind(X,myRarray1)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="25",background="white")
      tkgrid(t.d, row=2, column=0)

    }

    ResultsbyLevel=function()
    {
      Prior=round(e$CRM.2$prior,3)
      n.patient=NULL
      total.tox=NULL
      ptox=round(e$CRM.2$ptox,3)
      ptoxL=round(e$CRM.2$ptoxL,3)
      ptoxU=round(e$CRM.2$ptoxU,3)
      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }

      for ( i in 1 : Study$Nb_Dl)
      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}
        total.tox=c(total.tox,res2)
      }

      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, Nb_patient=n.patient,
                                     Nb_tox=total.tox, P_tox=ptox, P_toxL=ptoxL, P_toxU=ptoxU), envir = as.environment(pos))
      X=colnames(myRarray2)
      myRarray=rbind(X,myRarray2)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="15",background="white")
      tkgrid(t.d, row=2, column=0)
    }

    ExportRes=function()
    {
      myRarray1 <- data.frame(patient=Res$PT.data[,3],dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2],
                              followup=Res$PT.data[,4])

      Prior=round(e$CRM.2$prior,3)
      n.patient=NULL
      total.tox=NULL
      ptox=round(e$CRM.2$ptox,3)
      ptoxL=round(e$CRM.2$ptoxL,3)
      ptoxU=round(e$CRM.2$ptoxU,3)
      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }

      for ( i in 1 : Study$Nb_Dl)

      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}

        total.tox=c(total.tox,res2)
      }
      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=Prior, Nb_patient=n.patient,
                                     Nb_tox=total.tox, P_tox=ptox, P_toxL=ptoxL, P_toxU=ptoxU), envir = as.environment(pos))

      filef <- tclvalue(tkgetSaveFile())
      if (filef != "") {
        filesave=paste(filef,"xlsx",sep=".")
        pdf(paste(filef,"pdf",sep="."))
        GrapheResults()
        dev.off()
        pdf(paste(paste(filef,"2",sep=""),"pdf",sep="."))
        curvesIC()
        dev.off()

        wb <- createWorkbook()
        sheet1 <- addWorksheet(wb, sheetName="By patients")
        sheet2 <- addWorksheet(wb, sheetName="By dose level")
        style <- createStyle(fontSize = 12, fontColour = "black",
                             textDecoration = c("bold", "italic", "underline"),
                             halign = "center", valign = "center", border = "Bottom",
                             fgFill = "gray")

        setColWidths(wb, sheet1, cols=c(1,3,4), widths = 12.43)
        setColWidths(wb, sheet1, cols=2, widths = 14.43)
        setColWidths(wb, sheet2, cols=3, widths = 14.43)
        setColWidths(wb, sheet2, cols=c(1,2,4,5,6,7), widths = 10.43)

        writeDataTable(wb,sheet1,x=myRarray1)
        writeDataTable(wb,sheet2,x=myRarray2)

        addStyle(wb,sheet1,style,cols=1:4,rows=1)
        addStyle(wb,sheet2,style,cols=1:7,rows=1)

        saveWorkbook(wb, filesave, overwrite = T)
      }
    }

    load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    l1<-tklabel(Resultas,text="")
    l2<-tklabel(Resultas,text="")
    l3<-tklabel(Resultas,text="")
    l4<-tklabel(Resultas,text="")

    Titre.1=tklabel(Resultas,text="Patient Summary",font=PoliceTitre)
    Titre.2=tklabel(Resultas,text="Dose level Summary",font=PoliceTitre)
    Titre.3=tklabel(Resultas,text="Graphic",font=PoliceTitre)
    Titre.4=tklabel(Resultas,text="Export results",font=PoliceTitre)

    button1=tkbutton(Resultas ,text="Results",width=20,command=Resultsbypatient,bg="cornflower blue")
    button2=tkbutton(Resultas ,text="Results",width=20,command=ResultsbyLevel,bg="cornflower blue")
    button3=tkbutton(Resultas ,text="Graphic",width=20,command=Export.Graph,bg="cornflower blue")
    button3b=tkbutton(Resultas ,text="Dose-tox curves",width=20,command=Export.CurvesIC,bg="cornflower blue")
    button4=tkbutton(Resultas ,text="Export",width=20,command=ExportRes,bg="cornflower blue")

    tkgrid(l1, row=0, column=0)
    tkgrid(Titre.1, row=1, column=1, sticky="w")
    tkconfigure(Titre.1, font=PoliceTitre)
    tkgrid(button1, row=1, column=2)

    tkgrid(l2, row=2, column=1)
    tkgrid(Titre.2, row=3, column=1, sticky="w")
    tkconfigure(Titre.2, font=PoliceTitre)
    tkgrid(button2, row=3, column=2)

    tkgrid(l3, row=4, column=1)
    tkgrid(Titre.3, row=5, column=1, sticky="w")
    tkconfigure(Titre.3, font=PoliceTitre)
    tkgrid(button3, row=5, column=2)
    tkgrid(button3b, row=5, column=3)

    tkgrid(l4, row=6, column=1)
    tkgrid(Titre.4, row=7, column=1, sticky="w")
    tkgrid(button4, row=7, column=2)
    tkconfigure(button4,padx=2)
    tkconfigure(Titre.4, font=PoliceTitre)
  }

  .Include=function(...)
  {
    load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'))

    assign("inc", tktoplevel(), envir = as.environment(pos))
    tkwm.geometry(inc, "+550+270")

    assign("mm", tkframe(inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(mm)
    assign("Titre", tklabel(mm,text="Treated dose level",font=PoliceGenerale), envir = as.environment(pos))
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DL <- tclVar(Res$MTD)
    for ( i in 1: Study$Nb_Dl)
    {
      tkgrid(assign(paste('rDL',i,sep=''),tkradiobutton(mm,variable=rbValue.DL ,value=i, text=paste(i),font=PoliceGenerale)))
    }
    assign("inc.mm1", tkframe(inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(inc.mm1)
    Titre <- tklabel(inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i),font=PoliceGenerale)))
    }

    assign("Fup", tkframe(inc,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Fup)

    TitreFup <- tklabel(Fup,text="Follow-up Time")
    tkgrid(TitreFup, row=0, columnspan=2)

    val=tclVar(Res$ObWin)
    assign("Followup", tkentry(Fup,textvariable=val, width=10), envir = as.environment(pos))
    tkgrid(Followup)


    .Sauv=function(...)
    {

      assign("DL", c(Res$PT.data[,1],as.numeric(tclvalue((rbValue.DL)))), envir = as.environment(pos))
      assign("DLT", c(Res$PT.data[,2],as.numeric(tclvalue((rbValue.DLT)))), envir = as.environment(pos))
      assign("patid", c(Res$PT.data[,3],Res$Pat.included+1), envir = as.environment(pos))
      followup2=c(Res$PT.data[,4], as.numeric(tclvalue(tkget((Followup)))))

      Res$PT.data<-data.frame(DL=DL,DLT=DLT,patid=patid,followup=followup2)
      Res$Pat.included<-Res$Pat.included+1
      print(Res$PT.data)

      DATA=Res$PT.data

      if (nrow(DATA)!=0)
      {
        e$CRM.2 <- dfcrm::titecrm(prior=Study$DL, target=Study$Target, tox=DATA[,2],level=DATA[,1], conf.level = 0.95,
                                  model = paste(Study$Model.n), followup=DATA$followup, obswin=Res$ObWin, intcpt=Study$Intcpt,
                                  scale=Study$Sd)
        Res$MTD<-e$CRM.2$mtd
      }
      if (nrow(DATA)!=0){
        CRM.2 <- e$CRM.2
        save(Res,Study,CRM.2,file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'))}

      load(file =paste(paste(names_Study,'tcrm',sep='-'),'RData',sep='.'))
      tkdestroy(inc)
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))

      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }
      if(Res$Pat.included==1)
      {
        disabled.CRM()
        if (etatPrior==TRUE)
        {disabled.PriorCalib()}
        if (etatPrior==FALSE)
        {tkdestroy(PriorCal)}
      }
    }

    inc.Win_TiTECRM<- tkframe(inc,relief="groove",borderwidth=3)
    Save=tkbutton(inc.Win_TiTECRM,text="Validate",width=30,command=.Sauv,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(inc.Win_TiTECRM)

  }

  GrapheResults=function(...)
  {

    DATA=Res$PT.data
    n=length(DATA[,1])

    x=c(1:n)
    y=DATA[,1]
    z=DATA[,2]
    Data=data.frame(x=x,y=y,z=z)
    plot(x,y,pch=16,cex=2,col=(z+1),type='b',axes=FALSE,xlab="Patient number",ylab='Dose Level',main="Patients, Dose level and toxicity")
    axis(1, lwd = 2)
    axis(side=2,1:8, lwd = 2)
    legend("bottomright",inset = c(0.0, -0.19),  lwd=c(2.5,2.5),legend=c("Non Toxicity","Toxicity"), col = c(1,2), pch = 16,cex=0.8,xpd = NA)
  }

  curvesIC = function(...)
  {

    post.var <- e$CRM.2$post.var
    crit <- qnorm(0.5 + e$CRM.2$conf.level/2)
    doses=e$CRM.2$dosescaled
    Fempi=function(doses,beta){doses^(exp(beta))}
    Flogi=function(doses,beta){(1 + exp(-e$CRM.2$intcpt- exp(beta) * doses))^{-1}}
    Fmodel <- switch(e$CRM.2$model,empiric=Fempi, logistic=Flogi)
    plot(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),'l', lwd=2,xaxt="n",
         ylim=c(0, 1),xlab="Dose level", ylab="Proba. of DLT")
    axis(1, at = seq(1, length(e$CRM.2$prior), by = 1))
    abline(h=e$CRM.2$target,col="blue",lty=2,lwd=2)
    objtitle <-rbind("Dose-toxicity curve + 0.95_IC",paste(e$CRM.2$model, "model"))
    title(main=objtitle)
    legend('topright',c("Proba. of DLT", "IC(0.95)","target"),lty=c(1,1,2), col=c('black','red','blue'),lwd=2)
    points(c(1:length(e$CRM.2$prior)),Fmodel(doses,e$CRM.2$estimate),lwd=2)
    if (post.var >= 0) {
      lb <- e$CRM.2$estimate - crit * sqrt(post.var)
      ub <- e$CRM.2$estimate + crit * sqrt(post.var)

      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,lb),col="red",lwd=2)
      lines(c(1:length(e$CRM.2$prior)),Fmodel(doses,ub),col="red",lwd=2)
    }else{print('Confidence interval not calculated')}
  }

  Export.CurvesIC=function()
  {
    curvesIC()
  }


  Export.Graph=function()
  {
    GrapheResults()
  }

  NewCRM=function()
  {

    tkdestroy(Win_TiTECRM)
    frameInitCRM()
    PriorCalib()
  }


  disabled.CRM=function()
  {
    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Rec.mm0)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=2, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    Nb_Niv_Doses<- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding  ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Nam_ObWin<- tclVar(Res$ObWin)
    labagewin<-tklabel(Saisi, text="Observation window ",font=PoliceGenerale)
    ObWin<-tkentry(Saisi, width=10,textvariable=Nam_ObWin,state="disabled")
    tkgrid(labagewin, row=8,column=0, sticky="w")
    tkgrid(ObWin, row=8,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Names of study:",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(labage, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,state="disabled",bg="cornflower blue")
    tkgrid(Save.rec, row=1)

    .ReCap()
  }


  Open.CRM=function()
  {
    tkdestroy(Win_TiTECRM)
    frameInitCRM()
    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=2, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    Nb_Niv_Doses <- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5, textvariable=n.iter,state="disabled")
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w")
    tkgrid(NBpat, row=5,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    rb1.1 <- tkradiobutton(Saisi,state="disabled") ; rb2.1 <- tkradiobutton(Saisi,state="disabled")
    rbValue.M <- tclVar(Study$Model)
    tkconfigure(rb1.1,variable=rbValue.M,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1,variable=rbValue.M,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre, row=6,column=0, sticky="w")
    tkgrid(rb1.1, row=6,column=1, sticky="w")
    tkgrid(rb2.1, row=7,column=1, sticky="w")

    Tsd.<-tklabel(Saisi, text="Normal S. deviation  ",font=PoliceGenerale)
    n.iter <- tclVar(Study$Sd)
    sd.<-tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tsd., row=6,column=2, sticky="w")
    tkgrid(sd., row=6,column=3, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale)
    n.iter <- tclVar(Study$Intcpt)
    assign("intcpt.", tkentry(Saisi, width=10, textvariable=n.iter ,state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt., row=7,column=2, sticky="w")
    tkgrid(intcpt., row=7,column=3, sticky="w")

    Nam_ObWin<- tclVar(Res$ObWin)
    labagewin<-tklabel(Saisi, text="Observation window ",font=PoliceGenerale)
    ObWin<-tkentry(Saisi, width=10,textvariable=Nam_ObWin,state="disabled")
    tkgrid(labagewin, row=8,column=0, sticky="w")
    tkgrid(ObWin, row=8,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Names of study",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(Titre, row=9,column=0, sticky="w")
    tkgrid(names.S, row=9,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=20,state="disabled")
    tkgrid(Save.rec, row=37, column=1, columnspan=2, rowspan=2)
    .ReCap()
    .ReCap.Resultat()
  }
  WordExport=function (dframe=NULL) {

    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}
    docx( ) %>%
      addFlexTable(dframe %>%
                     FlexTable(header.cell.props = cellProperties( background.color = "#003366"),
                               header.text.props = textBold(color = "white"),
                               add.rownames = TRUE ) %>%
                     setZebraStyle(odd = "#DDDDDD", even = "#FFFFFF")) %>%
      writeDoc(file = "ResExport.docx")
    Save.word.infos <- tkmessageBox(title = "Word Export",
                                    message = "File ResExport.docx saved in the current working directory of the R proces", icon = "info", type = "ok")
  }

  ExcellExport=function (dframe=NULL) {
    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}

    filef_ <- tclvalue(tkgetSaveFile())
    if (filef_ != "") {
      filesave=paste(filef_,"xlsx",sep=".")

      wb_ <- createWorkbook()
      sheet1_ <- addWorksheet(wb_, sheetName="Simulation")

      style_ <- createStyle(fontSize = 12, fontColour = "black",
                            textDecoration = c("bold", "italic", "underline"),
                            halign = "center", valign = "center", border = "Bottom",
                            fgFill = "gray")

      setColWidths(wb_, sheet1_, cols=c(1,2,3,4,5), widths = 13.43)

      writeDataTable(wb_,sheet1_,x=dframe)

      addStyle(wb_,sheet1_,style_,cols=1:5,rows=1)

      saveWorkbook(wb_, filesave, overwrite = T)

    }
  }
  .RecapSim=function()
  {

    .Save_sim=function()
    {
      Study.file=function()
      {
        a00=paste(StudyS$Name)
        a0=paste('Name of study',paste(StudyS$Name),sep=':')
        a1=paste('Method','TITE-CRMB',sep=':')
        a2='Time-To-Event Continual Reassessment Method (TITE-CRM) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(StudyS$Name),'tcsim', sep='.'))
      }
      Study.file()
      ResSim <- e$ResSim
      save(ResSim,StudyS,file =paste(paste(StudyS$Name,'tcsim',sep='-'),'RData',sep='.'))
      Save.infos <- tkmessageBox(title = "Data storage Infos",
                                 message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

    assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.fr, row=0, column=0, rowspan=16)

    PriorRes=paste(round(e$ResSim$prior[1],3))
    TrueRes=paste(round(e$ResSim$PI[1],3))
    MTDRes=paste(round(e$ResSim$MTD[1],3))
    ToxRes=paste(round(e$ResSim$tox[1],3))
    LevelRes=paste(round(e$ResSim$level[1],3))
    for( i in 2 : length(e$ResSim$prior))
    {
      PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
      TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
      MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
      LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
      ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
    }



    NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")


    Titre1<-tklabel(Rec.fr,text="Results of the simulation")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
    aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
    aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
    aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
    aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
    aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
    aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
    aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
    aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
    aff_1.1<-tklabel(Rec.fr,text=paste("Time-o-Event Continued reassessment method "))
    aff_1.2<-tklabel(Rec.fr,text=paste("Bayesian approach (TITE-CRMB) "))
    aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
    aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
    aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
    aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
    aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
    aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
    aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
    aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
    aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
    aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
    aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
    aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))
    aff_12.0<-tklabel(Rec.fr,text=paste("Patient arrival rate (per obs. window)"))
    aff_12.1<-tklabel(Rec.fr,text=paste(StudyS$Rate))
    aff_13.0<-tklabel(Rec.fr,text=paste("Patient accrual scheme"))
    aff_13.1<-tklabel(Rec.fr,text=paste(StudyS$Accrual))

    tkgrid(aff_1.0, row=4, column=1,  sticky="w")
    tkgrid(aff_1.1, row=4, column=2,  sticky="w")
    tkgrid(aff_1.2, row=5, column=2,  sticky="w")
    tkconfigure(aff_1.0, font=Policeligne)
    tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
    tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

    tkgrid(aff_2.0, row=6, column=1, sticky="w")
    tkgrid(aff_2.1, row=6, column=2, sticky="w")
    tkconfigure(aff_2.0, font=Policeligne)
    tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

    tkgrid(aff_3.0, row=7, column=1, sticky="w")
    tkgrid(aff_3.1, row=7, column=2,  sticky="w")
    tkconfigure(aff_3.0, font=Policeligne )
    tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

    tkgrid(aff_4.0, row=8, column=1, sticky="w")
    tkgrid(aff_4.1, row=8, column=2,  sticky="w")
    tkconfigure(aff_4.0, font=Policeligne )
    tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

    tkgrid(aff_5.0, row=9, column=1,  sticky="w")
    tkgrid(aff_5.1, row=9, column=2,sticky="w")
    tkconfigure(aff_5.0, font=Policeligne )
    tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

    tkgrid(aff_6.0, row=10, column=1,  sticky="w")
    tkgrid(aff_6.1, row=10, column=2,  sticky="w")
    tkconfigure(aff_6.0, font=Policeligne)
    tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

    tkgrid(aff_7.0, row=11, column=1,  sticky="w")
    tkgrid(aff_7.1, row=11, column=2,  sticky="w")
    tkconfigure(aff_7.0, font=Policeligne )
    tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

    tkgrid(aff_11.0, row=12, column=1,  sticky="w")
    tkgrid(aff_11.1, row=12, column=2,  sticky="w")
    tkconfigure(aff_11.0, font=Policeligne )
    tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

    tkgrid(aff_12.0, row=13, column=1,  sticky="w")
    tkgrid(aff_12.1, row=13, column=2,  sticky="w")
    tkconfigure(aff_12.0, font=Policeligne )
    tkconfigure(aff_12.1, font=Policeligne , foreground="blue")

    tkgrid(aff_13.0, row=14, column=1,  sticky="w")
    tkgrid(aff_13.1, row=14, column=2,  sticky="w")
    tkconfigure(aff_13.0, font=Policeligne )
    tkconfigure(aff_13.1, font=Policeligne , foreground="blue")

    tkgrid(aff_8.0, row=15, column=1,  sticky="w")
    tkgrid(aff_8.1, row=15, column=2,  sticky="w")
    tkconfigure(aff_8.0, font=Policeligne )
    tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

    tkgrid(aff_9.0, row=16, column=1,  sticky="w")
    tkgrid(aff_9.1, row=16, column=2,  sticky="w")
    tkconfigure(aff_9.0, font=Policeligne )
    tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

    tkgrid(aff_10.0, row=17, column=1,  sticky="w")
    tkgrid(aff_10.1, row=17, column=2,  sticky="w")
    tkconfigure(aff_10.0, font=Policeligne )
    tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

    assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.save_, row=17, column=0)

    ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
    colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                         "Av. patient","Av. tox. ")
    WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                      command=function()ExcellExport(dframe=round(ResExp_,3)),bg="cornflower blue")
    tkgrid(WordExp_, row=1)
    blank<-tklabel(Rec.save_,text="      ")
    tkgrid(blank, row=2)

    SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,command=.Save_sim,bg="cornflower blue")
    tkgrid(SaveSim_, row=3)
  }

  disabled.SimCRM=function()
  {
    .RecapSimB=function()
    {
      .Save_sim=function()
      {
        Study.file=function()
        {
          a00=paste(StudyS$Name)
          a0=paste('Name of study',paste(StudyS$Name),sep=':')
          a1=paste('Method','TITE-CRMB',sep=':')
          a2='Continual Reassessment Method (CRM) for Phase I Clinical Trials'
          datee<-paste(format(Sys.time(), "%A %d %B %Y"))
          a3=paste('Date of creation',datee)
          Data=data.frame(c(a00,a0,a1,a2,a3))
          write.table(Data,paste(paste(StudyS$Name),'tcsim', sep='.'))
        }
        Study.file()
        ResSim <- e$ResSim
        save(ResSim,StudyS,file =paste(paste(StudyS$Name,'tcsim',sep='-'),'RData',sep='.'))
        Save.infos <- tkmessageBox(title = "Data storage Infos",
                                   message = "Files saved in the current working directory of the R proces", icon = "info", type = "ok")
      }
      PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
      PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

      assign("Rec.fr", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=16)

      PriorRes=paste(round(e$ResSim$prior[1],3))
      TrueRes=paste(round(e$ResSim$PI[1],3))
      MTDRes=paste(round(e$ResSim$MTD[1],3))
      ToxRes=paste(round(e$ResSim$tox[1],3))
      LevelRes=paste(round(e$ResSim$level[1],3))
      for( i in 2 : length(e$ResSim$prior))
      {
        PriorRes=paste(PriorRes,paste(round(e$ResSim$prior[i],3)),sep=", ")
        TrueRes=paste(TrueRes,paste(round(e$ResSim$PI[i],3)),sep=", ")
        MTDRes=paste(MTDRes,paste(round(e$ResSim$MTD[i],3)),sep=", ")
        LevelRes=paste(LevelRes,paste(round(e$ResSim$level[i],3)),sep=", ")
        ToxRes=paste(ToxRes,paste(round(e$ResSim$tox[i],3)),sep=", ")
      }

      NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
      tkgrid(NS, row=1, column=1,  sticky="w")
      tkconfigure(NS, font=PoliceEtude, foreground="white")

      Titre1<-tklabel(Rec.fr,text="Results of the simulation")
      tkgrid(Titre1, row=3, column=1)
      tkconfigure(Titre1, font=PoliceTitre)

      aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
      aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
      aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
      aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
      aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
      aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
      aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
      aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
      aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
      aff_1.1<-tklabel(Rec.fr,text=paste("Time-to-Event Continued reassessment method "))
      aff_1.2<-tklabel(Rec.fr,text=paste("Bayesian approach (TITE-CRMB) "))
      aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
      aff_6.1<-tklabel(Rec.fr,text=paste(StudyS$Model))
      aff_8.0<-tklabel(Rec.fr,text=paste("Distribution of the MTD estimates"))
      aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
      aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
      aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$n))
      aff_9.0<-tklabel(Rec.fr,text=paste("Average number of patients treated by dose level"))
      aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
      aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
      aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
      aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
      aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))
      aff_12.0<-tklabel(Rec.fr,text=paste("Patient arrival rate (per obs. window)"))
      aff_12.1<-tklabel(Rec.fr,text=paste(StudyS$Rate))
      aff_13.0<-tklabel(Rec.fr,text=paste("Patient accrual scheme"))
      aff_13.1<-tklabel(Rec.fr,text=paste(StudyS$Accrual))

      tkgrid(aff_1.0, row=4, column=1,  sticky="w")
      tkgrid(aff_1.1, row=4, column=2,  sticky="w")
      tkgrid(aff_1.2, row=5, column=2,  sticky="w")
      tkconfigure(aff_1.0, font=Policeligne)
      tkconfigure(aff_1.1, font=Policeligne , foreground="blue")
      tkconfigure(aff_1.2, font=Policeligne , foreground="blue")

      tkgrid(aff_2.0, row=6, column=1, sticky="w")
      tkgrid(aff_2.1, row=6, column=2, sticky="w")
      tkconfigure(aff_2.0, font=Policeligne)
      tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

      tkgrid(aff_3.0, row=7, column=1, sticky="w")
      tkgrid(aff_3.1, row=7, column=2,  sticky="w")
      tkconfigure(aff_3.0, font=Policeligne )
      tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

      tkgrid(aff_4.0, row=8, column=1, sticky="w")
      tkgrid(aff_4.1, row=8, column=2,  sticky="w")
      tkconfigure(aff_4.0, font=Policeligne )
      tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

      tkgrid(aff_5.0, row=9, column=1,  sticky="w")
      tkgrid(aff_5.1, row=9, column=2,sticky="w")
      tkconfigure(aff_5.0, font=Policeligne )
      tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

      tkgrid(aff_6.0, row=10, column=1,  sticky="w")
      tkgrid(aff_6.1, row=10, column=2,  sticky="w")
      tkconfigure(aff_6.0, font=Policeligne)
      tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

      tkgrid(aff_7.0, row=11, column=1,  sticky="w")
      tkgrid(aff_7.1, row=11, column=2,  sticky="w")
      tkconfigure(aff_7.0, font=Policeligne )
      tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

      tkgrid(aff_11.0, row=12, column=1,  sticky="w")
      tkgrid(aff_11.1, row=12, column=2,  sticky="w")
      tkconfigure(aff_11.0, font=Policeligne )
      tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

      tkgrid(aff_12.0, row=13, column=1,  sticky="w")
      tkgrid(aff_12.1, row=13, column=2,  sticky="w")
      tkconfigure(aff_12.0, font=Policeligne )
      tkconfigure(aff_12.1, font=Policeligne , foreground="blue")

      tkgrid(aff_13.0, row=14, column=1,  sticky="w")
      tkgrid(aff_13.1, row=14, column=2,  sticky="w")
      tkconfigure(aff_13.0, font=Policeligne )
      tkconfigure(aff_13.1, font=Policeligne , foreground="blue")

      tkgrid(aff_8.0, row=15, column=1,  sticky="w")
      tkgrid(aff_8.1, row=15, column=2,  sticky="w")
      tkconfigure(aff_8.0, font=Policeligne )
      tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

      tkgrid(aff_9.0, row=16, column=1,  sticky="w")
      tkgrid(aff_9.1, row=16, column=2,  sticky="w")
      tkconfigure(aff_9.0, font=Policeligne )
      tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

      tkgrid(aff_10.0, row=17, column=1,  sticky="w")
      tkgrid(aff_10.1, row=17, column=2,  sticky="w")
      tkconfigure(aff_10.0, font=Policeligne )
      tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

      assign("Rec.save_", tkframe(Resultas_, borderwidth=3), envir = as.environment(pos))
      tkgrid(Rec.save_, row=17, column=0)

      ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,e$ResSim$MTD,e$ResSim$level,e$ResSim$tox))
      colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.",
                           "Av. patient","Av. tox. ")
      WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                        command=function()ExcellExport(dframe=round(ResExp_,3)),bg="cornflower blue")
      tkgrid(WordExp_, row=1)
      blank<-tklabel(Rec.save_,text="      ")
      tkgrid(blank, row=2)

      SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,state="disabled",bg="cornflower blue")
      tkgrid(SaveSim_, row=3)
    }

    tkdestroy(Saisi_)
    tkdestroy(m2_)
    tkdestroy(Run_)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)


    Nb_Niv_Doses <- StudyS$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (StudyS$Prior[i])
      n.itert<- tclVar (StudyS$True[i])


      tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8, textvariable=n.itert,state="disabled"),envir = as.environment(pos)),row=r,column=1)

      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=3)

    }

    SliderValue_=tclVar(StudyS$Nb_Dl)
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Target)
    Target_<-tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled")
    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Npat)
    assign("n_", tkentry(Saisi_, width=5, textvariable=n.iter ,state="disabled"), envir = as.environment(pos))
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Nsim)
    Nsim_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$X0)
    X0_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Seed)
    Seed_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model", font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_,state="disabled") ; rb2.1_ <- tkradiobutton(Saisi_,state="disabled")
    if (StudyS$Model == "logistic") {
      rbValue.M_ <- tclVar(1)
      n.intcpt <- tclVar(StudyS$Intcpt)}
    if (StudyS$Model == "empiric") {
      rbValue.M_ <- tclVar(2)
      n.intcpt <- tclVar("NA")}
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal S. deviation  ", font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Sd)
    sd_<-tkentry(Saisi_, width=7, textvariable=n.iter, state="disabled")
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept", font=PoliceGenerale)
    assign("intcpt_", tkentry(Saisi_, width=7, textvariable=n.intcpt, state="disabled"), envir = as.environment(pos))
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Tobsw_<-tklabel(Saisi_, text="Observation window",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Obswin)
    Obsw_<-tkentry(Saisi_, width=10,textvariable=n.iter ,state="disabled")
    tkgrid(Tobsw_, row=9,column=0, sticky="w")
    tkgrid(Obsw_, row=9,column=1, sticky="w")

    Trate_<-tklabel(Saisi_, text="Expected number of arrivals per
                    observation window",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Rate)
    Rate_<-tkentry(Saisi_, width=10,textvariable=n.iter ,state="disabled")
    tkgrid(Trate_, row=10,column=0, sticky="w")
    tkgrid(Rate_, row=10,column=1, sticky="w")

    Taccrual_=tklabel(Saisi_,text="Patient accrual scheme",font=PoliceGenerale)
    rbacc.1_ <- tkradiobutton(Saisi_,state="disabled") ; rbacc.2_ <- tkradiobutton(Saisi_,state="disabled")
    if (StudyS$Accrual == "fixed") {rbAcc.M_ <- tclVar(1)}
    if (StudyS$Accrual == "poisson") {rbAcc.M_ <- tclVar(2)}

    tkconfigure(rbacc.1_,variable=rbAcc.M_,value=1, text="fixed",font=PoliceGenerale)
    tkconfigure(rbacc.2_,variable=rbAcc.M_,value=2, text="poisson",font=PoliceGenerale)

    tkgrid(Taccrual_, row=11,column=0, sticky="w")
    tkgrid(rbacc.1_, row=11,column=1, sticky="w")
    tkgrid(rbacc.2_, row=12,column=1, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_,state="disabled") ;
    const2_ <- tkradiobutton(Saisi_,state="disabled")
    rbValue.const_ <- tclVar(as.numeric(StudyS$Constrain))
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=11, column=2, sticky="w")
    tkgrid(const1_, row=11,  column=3, sticky="w")
    tkgrid(const2_, row=12,column=3, sticky="w")

    Tstop_=tklabel(Saisi_,text="At the moment an unique stopping rule is proposed: sample size",font=PoliceGenerale)
    tkgrid(Tstop_, row=13,column=0,columnspan=4, sticky="w")


    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(StudyS$Name)
    names.S_<-tkentry(Saisi_, width=20,textvariable=Nam_,state="disabled")
    tkgrid(labage_, row=14,column=0, sticky="w")
    tkgrid(names.S_,row=14,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,state="disabled",bg="cornflower blue")
    tkgrid(RunSim_, row=1)

    .RecapSimB()
  }

  SimCRM=function()
  {
    tkdestroy(Win_TiTECRM)
    frameInitSIM()

    .Crea.f_=function(...)
    {
      Nb_Niv_Doses_<-as.numeric(tclvalue((SliderValue_)))
      assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
      tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)

      for ( i in 1: Nb_Niv_Doses_)
      {
        r=17 + i
        tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=1)

        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=3)
      }
      if( Nb_Niv_Doses_!=8)
      {
        for ( i in (Nb_Niv_Doses_+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)

          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=2)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=3)
        }
      }
    }

    .Param.c_= function(...)
    {
      if (as.numeric(tclvalue((rbValue.M_)))==2)
      {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt_", tkentry(Saisi_, width=7, state="disabled"), envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")

      }
      else {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
        assign("intcpt_", tkentry(Saisi_, width=7), envir = as.environment(pos))
        tkgrid(Tintcpt_, row=8,column=2, sticky="w")
        tkgrid(intcpt_, row=8,column=3, sticky="w")
      }
    }

    .Simulation_=function (...)
    {
      .Sauvgarde.Study_=function(...)
      {
        Nb_Dl=as.numeric(tclvalue(SliderValue_))
        prior=NULL
        true=NULL
        if (Nb_Dl==2) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))))}
        if (Nb_Dl==3) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))))}
        if (Nb_Dl==4) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))))}
        if (Nb_Dl==5) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))))}
        if (Nb_Dl==6) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))))}
        if (Nb_Dl==7) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))))}
        if (Nb_Dl==8) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))),as.numeric(tclvalue(tkget(valt8))))}

        target=as.numeric(tclvalue(tkget((Target_))))

        Model=as.numeric(tclvalue((rbValue.M_)))
        Const=as.logical(as.numeric(tclvalue((rbValue.const_))))
        npat=as.numeric(tclvalue(tkget(n_)))
        nsim=as.numeric(tclvalue(tkget((Nsim_))))
        x0=as.numeric(tclvalue(tkget((X0_))))
        obswin=as.numeric(tclvalue(tkget((Obsw_))))
        rate=as.numeric(tclvalue(tkget((Rate_))))
        Accrual=as.numeric(tclvalue((rbAcc.M_)))
        seed=as.numeric(tclvalue(tkget((Seed_))))
        sdv=as.numeric(tclvalue(tkget((sd_))))
        assign("Names_Study", tclvalue(tkget(names.S_)), envir = as.environment(pos))
        if (Model == 1) {model=as.character("logistic"); intcp=as.numeric(tclvalue(tkget((intcpt_))))}
        if (Model == 2) {model=as.character("empiric");intcp=NULL}
        if (Accrual == 1) {accrual=as.character("fixed")}
        if (Accrual == 2) {accrual=as.character("poisson")}
        res.users <- 1
        if (target>0.55) cat("\n Warning: Target DLT rate too high")
        if (target<0.15) cat("\n Warning: Target DLT rate too low")
        if (npat <=0) stop('Number of patients to be enrolled <=0')
        if (npat > 100) cat("\n Warning: Number of patients to be enrolled > 100")
        if ((x0 <1)|(x0 > Nb_Dl)|(x0%%1 != 0)) stop(paste('Starting dose level incorrect, enter an integer between 1 and',Nb_Dl))
        if (obswin <= 0 ) stop('Observation window incorrect')
        if (rate <= 0 ) stop('Expected number of arrivals per observation window must be strictly positive')
        if (nsim < 100) cat("\n Warning: Low number of simulations")
        check_nsim <- function (){
          res.users <- 2
          while (res.users != 0 & res.users != 1) {
            cat("\n Warning: Large number of simulations, continue?  y or n")
            yn <- readLines(n=1)
            if(yn == "y" | yn == "Y" | yn== "yes" | yn == "Yes"){res.users <- 1;}
            if(yn == "n" | yn == "N" | yn== "no" | yn == "No"){res.users <- 0;}
          }
          return(res.users)
        }

        if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
        if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
        if (any(true <= 0) | any(true >= 1)) stop('True probabilities must be within ]0,1[ ')
        if (is.unsorted(true, strictly = TRUE)) stop('True probabilities should be monotonically increasing')
        if (nsim > 10000)  {res.users <- check_nsim()}
        assign("StudyS", list(Name=paste(Names_Study),Nb_Dl=Nb_Dl,True=true,Prior=prior,Target=target,
                              Model=model,Npat=npat,Nsim=nsim,X0=x0,Obswin=obswin,
                              Rate=rate,Accrual=accrual, Intcpt=intcp, Sd=sdv, Constrain=Const, Seed=seed),
               envir = as.environment(pos))
        if (res.users == 1) {
          cat("\n Submission in progress... Please wait... ", "\n")
          e$ResSim <- dfcrm::titesim(PI=StudyS$True, prior=StudyS$Prior,target=StudyS$Target,n=StudyS$Npat,
                                     x0=StudyS$X0,nsim=StudyS$Nsim, obswin=StudyS$Obswin, rate=StudyS$Rate,
                                     accrual=StudyS$Accrual, method="bayes",model=StudyS$Model,count=FALSE,
                                     intcpt=StudyS$Intcpt, scale=StudyS$Sd, restrict=StudyS$Constrain, seed=StudyS$Seed)
          .RecapSim()
          tk2notetab.select(net.TiTECRM_, "Results")
        }

      }
      .Sauvgarde.Study_()
    }


    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    SliderValue_=tclVar('8')
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",command=.Crea.f_)
    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    assign("Target_", tkentry(Saisi_, width=5), envir = as.environment(pos))

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    l<-tklabel(Saisi_,text="            ")
    tkgrid(l, row=2, column=3)

    Titre_=tklabel(Saisi_,text="Sample size",font=PoliceGenerale)
    assign("n_", tkentry(Saisi_, width=5), envir = as.environment(pos))
    tkgrid(Titre_, row=3, column=0, sticky="w")
    tkgrid(n_, row=3, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    Nsim_<-tkentry(Saisi_, width=10)
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    X0_<-tkentry(Saisi_, width=10)
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    Seed_<-tkentry(Saisi_, width=10)
    tkgrid(Tseed_, row=6,column=0, sticky="w")
    tkgrid(Seed_, row=6,column=1, sticky="w")

    Titre_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    rb1.1_ <- tkradiobutton(Saisi_,command=.Param.c_) ; rb2.1_ <- tkradiobutton(Saisi_,command=.Param.c_)
    rbValue.M_ <- tclVar(1)
    tkconfigure(rb1.1_,variable=rbValue.M_,value=1, text="Logistic",font=PoliceGenerale)
    tkconfigure(rb2.1_,variable=rbValue.M_,value=2, text="Empiric",font=PoliceGenerale)

    tkgrid(Titre_, row=7,column=0, sticky="w")
    tkgrid(rb1.1_, row=7,column=1, sticky="w")
    tkgrid(rb2.1_, row=8,column=1, sticky="w")

    Tsd_<-tklabel(Saisi_, text="Normal S. deviation",font=PoliceGenerale)
    sd_<-tkentry(Saisi_, width=7)
    tkgrid(Tsd_, row=7,column=2, sticky="w")
    tkgrid(sd_, row=7,column=3, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
    assign("intcpt_", tkentry(Saisi_, width=7), envir = as.environment(pos))
    tkgrid(Tintcpt_, row=8,column=2, sticky="w")
    tkgrid(intcpt_, row=8,column=3, sticky="w")

    Tobsw_<-tklabel(Saisi_, text="Observation window",font=PoliceGenerale)
    Obsw_<-tkentry(Saisi_, width=10)
    tkgrid(Tobsw_, row=9,column=0, sticky="w")
    tkgrid(Obsw_, row=9,column=1, sticky="w")

    Trate_<-tklabel(Saisi_, text="Expected number of arrivals per
                    observation window",font=PoliceGenerale)
    Rate_<-tkentry(Saisi_, width=10)
    tkgrid(Trate_, row=10,column=0, sticky="w")
    tkgrid(Rate_, row=10,column=1, sticky="w")

    Taccrual_=tklabel(Saisi_,text="Patient accrual scheme",font=PoliceGenerale)
    rbacc.1_ <- tkradiobutton(Saisi_) ; rbacc.2_ <- tkradiobutton(Saisi_)
    rbAcc.M_ <- tclVar(1)
    tkconfigure(rbacc.1_,variable=rbAcc.M_,value=1, text="fixed",font=PoliceGenerale)
    tkconfigure(rbacc.2_,variable=rbAcc.M_,value=2, text="poisson",font=PoliceGenerale)

    tkgrid(Taccrual_, row=11,column=0, sticky="w")
    tkgrid(rbacc.1_, row=11,column=1, sticky="w")
    tkgrid(rbacc.2_, row=12,column=1, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_) ; const2_ <- tkradiobutton(Saisi_)
    rbValue.const_ <- tclVar(1)
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=11, column=2, sticky="w")
    tkgrid(const1_, row=11,  column=3, sticky="w")
    tkgrid(const2_, row=12,column=3, sticky="w")

    Tstop_=tklabel(Saisi_,text="At the moment an unique stopping rule is proposed: sample size",font=PoliceGenerale)
    tkgrid(Tstop_, row=13,column=0,columnspan=4, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    names.S_<-tkentry(Saisi_, width=20)

    tkgrid(labage_, row=14,column=0, sticky="w")
    tkgrid(names.S_,row=14,column=1, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,command=.Simulation_,bg="cornflower blue")
    tkgrid(RunSim_, row=1)

  }

  save_Qf2=function(){ tkmessageBox(title = "Data storage Infos",
                                    message = "Data are automatically saved after each inclusion", icon = "info", type = "ok")}
  save_Qf=function(){ tkmessageBox(title = "Data storage Infos",
                                   message = "Please use the button save study available in interactive TITECRM or simulator to save your data", icon = "info", type = "ok")}
  about=function(){ tkmessageBox(title = "Information",
                                 message = "Time-to-Event Continual Reassessment Method Bayesian Interface 2017",
                                 icon = "info", type = "ok")}

  Open.help=function() {browseURL("https://cran.r-project.org/package=dfcrm", browser=getOption("browser"),
                                  encodeIfNeeded = FALSE) }



  frameInit=function(){

    assign("Win_TiTECRM", tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_TiTECRM, "630x500")
    tkwm.geometry(Win_TiTECRM, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TiTECRM, ilogo)
    tktitle(Win_TiTECRM) <- "GUI TiTE-CRM"
    tkpack.propagate(Win_TiTECRM, FALSE)
    topMenu <- tk2menu(Win_TiTECRM)
    tkconfigure(Win_TiTECRM, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive TiTE-CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="TiTE-CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){tkdestroy(Win_TiTECRM);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TiTECRM))

  }


  frameInitCRM=function(){

    assign("Win_TiTECRM", tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_TiTECRM, "680x575")
    tkwm.geometry(Win_TiTECRM, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TiTECRM, ilogo)
    tktitle(Win_TiTECRM) <- "GUi TiTECRM"
    tkpack.propagate(Win_TiTECRM, FALSE)
    names_Study=''

    topMenu <- tk2menu(Win_TiTECRM)
    tkconfigure(Win_TiTECRM, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive Tite-CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="Tite-CRM simulator", command=SimCRM)


    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){tkdestroy(Win_TiTECRM);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf2)
    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TiTECRM))


    assign("net.TiTECRM", tk2notebook(Win_TiTECRM, tabs = c("Prior calibration","Input parameters",
                                                            "Include","Results")), envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.TiTECRM , fill = "both", expand = 1)

    assign("Include", tk2notetab(net.TiTECRM, "Include"), envir = as.environment(pos))
    tkpack.propagate(Include, FALSE)

    assign("Resultas", tk2notetab(net.TiTECRM, "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas, FALSE)

    assign("Required", tk2notetab(net.TiTECRM, "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required, FALSE)

    assign("PriorCal", tk2notetab(net.TiTECRM, "Prior calibration"), envir = as.environment(pos))
    tkpack.propagate(PriorCal, FALSE)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Rec.Win_TiTECRM", tkframe(Include,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.Win_TiTECRM, row=9, column=2, rowspan=16)

    assign("Rec.mm0",tkframe(Include, borderwidth=3), envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, columnspan=3, rowspan=16)

    assign("halfwidthPrior", NULL, envir = as.environment(pos))
    assign("nuPrior", NULL, envir = as.environment(pos))
    assign("etatPrior", FALSE, envir = as.environment(pos))
  }

  frameInitSIM=function(){

    assign("Win_TiTECRM", tktoplevel(background = "light steel blue"), envir = as.environment(pos))
    tkwm.geometry(Win_TiTECRM, "690x690")
    tkwm.geometry(Win_TiTECRM, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TiTECRM, ilogo)
    tktitle(Win_TiTECRM) <- "GUi TiTECRM"
    tkpack.propagate(Win_TiTECRM, FALSE)
    names_Study=''

    topMenu <- tk2menu(Win_TiTECRM)
    tkconfigure(Win_TiTECRM, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)

    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)

    QuiterMenu <- tk2menu(topMenu, tearoff=FALSE)
    tkadd(topMenu,"cascade", label="Exit", menu=QuiterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive Tite-CRM", command=NewCRM)
    tkadd(newStudyMenu,"command", label="Tite-CRM simulator", command=SimCRM)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){tkdestroy(Win_TiTECRM);frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuiterMenu,"command", label="Save_Quit", command=save_Qf)

    tkadd(QuiterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TiTECRM))


    assign("net.TiTECRM_", tk2notebook(Win_TiTECRM, tabs = c("Input parameters","Results")),
           envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.TiTECRM_ , fill = "both", expand = 1)

    assign("Resultas_", tk2notetab(net.TiTECRM_ , "Results"), envir = as.environment(pos))
    tkpack.propagate(Resultas_, FALSE)

    assign("Required_", tk2notetab(net.TiTECRM_ , "Input parameters"), envir = as.environment(pos))
    tkpack.propagate(Required_, FALSE)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2), envir = as.environment(pos))
    tkpack.propagate(m2_, FALSE)
  }
  frameInit()
}

.EWOC=function(pos=1, e)
{
  e$EWOC.2 = NULL
  e$ResSim = NULL
  tclRequire("Tktable")
  loads <- function()
  {

    loads.infos <- tkmessageBox(title = "Data loading Infos",
                                message = "Select a file.ewoc or file.ewsim", icon = "info", type = "ok")
    filef <- tclvalue(tkgetOpenFile())
    if (filef != "") {
      stu=read.table(filef,header=TRUE)
      assign("names_Study", paste(stu[1,]), envir = as.environment(pos))
      extension<-str_sub(filef,-4)
    }
    if (extension == "ewoc") {
      load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      assign("Res", Res, envir = as.environment(pos))
      assign("Study", Study, envir = as.environment(pos))
      e$EWOC.2 <- EWOC.2
      Open.EWOC()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          tkdestroy(include)
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            tkdestroy(include)
          }

        }
      }
    }
    if (extension == "wsim") {
      load(file =paste(paste(names_Study,'ewsim',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      e$ResSim <- ResSim
      assign("StudyS", StudyS, envir = as.environment(pos))
      tkdestroy(Win_EWOC)
      frameInitSIM()
      disabled.SimEWOC()
    }
  }

  pending_listing=function()
  {
    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))
    dataa=Res$PT.data[which(Res$PT.data$pndg==1),]

    if (nrow(dataa)==0 )
    {
      msg <- paste("No patient pending",sep="")
      tkmessageBox(message=msg)
    }

    if (nrow(dataa)>=1)
    {
      assign("tt.pend", tktoplevel(),envir = as.environment(pos))
      tkwm.geometry(tt.pend, "+650+400")
      assign("tl", tklistbox(tt.pend,height=4,selectmode="single",background="white"),envir = as.environment(pos))
      tkgrid(tklabel(tt.pend,text="Select a patient",font=PoliceGenerale))
      tkgrid(tl)
      numPatP=paste("Patient N_", as.character(dataa$patid),sep="")
      for (i in (1:length(numPatP)))
      {
        tkinsert(tl,"end",numPatP[i])
      }
      tkselection.set(tl,0)
      OK.but <-tkbutton(tt.pend,text="  OK  ",command=.include.pending,bg="cornflower blue")
      tkgrid(OK.but)
      tkfocus(tt.pend)
    }
  }
  .include.pending <- function()
  {
    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))
    data.pinding=Res$PT.data[which(Res$PT.data$pndg==1),]
    data.inclus=Res$PT.data[which(Res$PT.data$pndg==0),]
    numPatP=as.character(data.pinding$patid)
    assign("Choice", as.numeric(numPatP[as.numeric(tkcurselection(tl))+1]),envir = as.environment(pos))

    Inc=tktoplevel()
    tkwm.geometry(Inc, "+650+400")

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkgrid(Inc.mm1)
    Titre <- tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    assign("rbValue.DLT", tclVar(0),envir = as.environment(pos))
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i))))
    }

    .Sauv2=function(...)
    {
      DLT <- as.numeric(tclvalue((rbValue.DLT)))
      Newpat=data.pinding[which(data.pinding$patid==Choice),]

      Newpat$DLT=DLT
      Newpat$pndg=0
      DATA=rbind(data.inclus,Newpat)

      data.ewoc=DATA[c(3,1,2)]
      names(data.ewoc)=c('patient', 'dose','tox')
      a=dim(data.ewoc)[1]
      if (a>1) {data.ewoc$patient<-c(1:a)}

      e$EWOC.2 <- bcrm::bcrm(stop=list(nmax=1),data=data.ewoc,p.tox0=Study$DL,simulate=FALSE,
                             ff=paste(Study$Model),constrain=Study$Constrain,cohort=1,
                             prior.alpha=list(Study$Prior.c,Study$a,Study$b),target.tox=Study$Target,
                             pointest=Study$Pointest,method="rjags")
      Res$MTD <- e$EWOC.2$ndose[[1]]$ndose
      Res$PT.data[which(Res$PT.data$patid==Choice),]$pndg=0
      Res$PT.data[which(Res$PT.data$patid==Choice),]$DLT=DLT
      EWOC.2 <- e$EWOC.2
      save(Res,Study, EWOC.2,file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))
      load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))

      Res
      tkdestroy(Inc)
      assign("Res", Res,envir = as.environment(pos))

      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }

        }

      }
      if (Res$Pat.included==1){disabled.EWOC()}
    }

    Inc.Win_EWOC<- tkframe(Inc,relief="groove",borderwidth=3)
    Save=tkbutton(Inc.Win_EWOC,text="Validate",width=30,command=.Sauv2,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_EWOC)
    tkdestroy (tt.pend)
  }

  .ReCap=function(...)
  {

    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))
    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(include, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Escalation with overdose control (EWOC) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model=="ht","Hyperbolic tangent",ifelse(Study$Model=="logit1","Logistic","Power"))))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_EWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
    Save=tkbutton(Rec.Win_EWOC,text="New patient",width=30,command=.include,bg="cornflower blue")
    Save.pend=tkbutton(Rec.Win_EWOC,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

    tkgrid(Rec.Win_EWOC, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    tkgrid(Save.pend, row=0, column=1)
  }

  disabled.ReCap=function(...)
  {
    tkdestroy(Rec.mm0)
    tkdestroy(Rec.Win_EWOC)
    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))
    LStudyDL=paste(Study$DL[1])
    for( i in 2 : length(Study$DL))
    {
      LStudyDL=paste(LStudyDL,paste(Study$DL[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(include, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(LStudyDL))


    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Escalation with overdose control (EWOC) "))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model=="ht","Hyperbolic tangent",ifelse(Study$Model=="logit1","Logistic","Power"))))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Stop==1,"Maximum number of patients to be enrolled in the study","Maximum number of patients by dose level")))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$N_Pat))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="Inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"),font=PoliceGenerale)
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included),font=PoliceGenerale)

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"),font=PoliceGenerale)
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD),font=PoliceGenerale)

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    DATA=Res$PT.data[which(Res$PT.data$pndg==1),]
    if (nrow(DATA)==0)
    {
      assign("Rec.Win_EWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
      Save=tkbutton(Rec.Win_EWOC,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_EWOC,text="Pending patient",width=30,state="disabled",bg="cornflower blue")
      tk2notetab.select(net.EWOC , "Results")

      tkgrid(Rec.Win_EWOC, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
    }
    if (nrow(DATA)!=0)
    {
      assign("Rec.Win_EWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
      Save=tkbutton(Rec.Win_EWOC,text="New patient",width=30, state="disabled",bg="cornflower blue")
      Save.pend=tkbutton(Rec.Win_EWOC,text="Pending patient",width=30,command=pending_listing,bg="cornflower blue")

      tkgrid(Rec.Win_EWOC, row=16, column=0)
      tkgrid(Save, row=0, column=0)
      tkgrid(Save.pend, row=0, column=1)
    }
  }

  .ReCap.Resultat=function(...)
  {
    Resultsbypatient=function()
    {
      assign("myRarray1", data.frame(patient=c(1:Res$Pat.included),
                                     dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2]),envir = as.environment(pos))
      X=colnames(myRarray1)
      myRarray=rbind(X,myRarray1)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat<-tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="25",background="white")
      tkgrid(t.d, row=2, column=0)

    }
    ResultsbyLevel=function()
    {
      Sdose=round(e$EWOC.2$ndose[[1]]$est,3)
      n.patient=NULL
      total.tox=NULL
      Mean=round(e$EWOC.2$ndose[[1]]$mean,3)
      SD=round(e$EWOC.2$ndose[[1]]$sd,3)
      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }
      for (i in 1 : Study$Nb_Dl)
      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}
        total.tox=c(total.tox,res2)
      }

      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Sdose=Sdose, n.patient=n.patient,
                                     total.tox=total.tox, Post.mean=Mean, Post.sd=SD), envir = as.environment(pos))
      X=colnames(myRarray2)
      myRarray=rbind(X,myRarray2)

      a=dim(myRarray)[2]-1
      b=dim(myRarray)[1]-1

      for (i in (0:b))
        for (j in (0:a))
          .Tcl(paste("set tclarray(",i,",",j,") ",myRarray[i+1,j+1],sep=""))

      Resbypat <- tktoplevel()
      t.d <- tkwidget(Resbypat,"table",variable="tclarray",rows=b+1,cols=a+1,titlerows="1",state="disabled",selectmode="extended",colwidth="25",background="white")
      tkgrid(t.d, row=2, column=0)
    }

    ExportRes=function()
    {
      myRarray1 <- data.frame(patient=c(1:Res$Pat.included),dose_level=Res$PT.data[,1],Toxicity=Res$PT.data[,2])

      Sdose=round(e$EWOC.2$ndose[[1]]$est,3)
      n.patient=NULL
      total.tox=NULL
      Mean=round(e$EWOC.2$ndose[[1]]$mean,3)
      SD=round(e$EWOC.2$ndose[[1]]$sd,3)

      for ( i in 1 : Study$Nb_Dl)
      {
        res1=sum(Res$PT.data[,1]==i)
        n.patient=c(n.patient,res1)
      }
      for ( i in 1 :Study$Nb_Dl)
      {
        res0=(Res$PT.data[which(Res$PT.data[,1]==i),])
        if (length(res0)==2) {res2=sum(res0[2]==1)} else {res2=sum(res0[,2]==1)}

        total.tox=c(total.tox,res2)
      }
      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Sdose=Sdose, n.patient=n.patient,
                                     total.tox=total.tox, Post.mean=Mean, Post.sd=SD),envir = as.environment(pos))

      filef <- tclvalue(tkgetSaveFile())
      if (filef != "") {
        filesave=paste(filef,"xlsx",sep=".")
        pdf(paste(filef,"pdf",sep="."))
        plot(e$EWOC.2)
        dev.off()
        wb <- createWorkbook()
        sheet1 <- addWorksheet(wb, sheetName="By patients")
        sheet2 <- addWorksheet(wb, sheetName="By dose level")
        style <- createStyle(fontSize = 12, fontColour = "black",
                             textDecoration = c("bold", "italic", "underline"),
                             halign = "center", valign = "center", border = "Bottom",
                             fgFill = "gray")

        setColWidths(wb, sheet1, cols=1:3, widths = 14.43)
        setColWidths(wb, sheet2, cols=1:6, widths = 14.43)

        writeDataTable(wb,sheet1,x=myRarray1)
        writeDataTable(wb,sheet2,x=myRarray2)

        addStyle(wb,sheet1,style,cols=1:3,rows=1)
        addStyle(wb,sheet2,style,cols=1:6,rows=1)

        saveWorkbook(wb, filesave, overwrite = T)
      }
    }
    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    l1<-tklabel(Resultas,text="")
    l2<-tklabel(Resultas,text="")
    l3<-tklabel(Resultas,text="")
    l4<-tklabel(Resultas,text="")


    Titre.1=tklabel(Resultas,text="Patient Summary",font=PoliceTitre)
    Titre.2=tklabel(Resultas,text="Dose level Summary",font=PoliceTitre)
    Titre.3=tklabel(Resultas,text="Graphic",font=PoliceTitre)
    Titre.4=tklabel(Resultas,text="Export results",font=PoliceTitre)

    button1=tkbutton(Resultas ,text="Results",width=20,command=Resultsbypatient,bg="cornflower blue")
    button2=tkbutton(Resultas ,text="Results",width=20,command=ResultsbyLevel,bg="cornflower blue")
    button3=tkbutton(Resultas ,text="Graphic",width=20,command=Export.Graph,bg="cornflower blue")
    button4=tkbutton(Resultas ,text="Export",width=20,command=ExportRes,bg="cornflower blue")

    tkgrid(l1, row=0, column=0)
    tkgrid(Titre.1, row=1, column=1, sticky="w")
    tkconfigure(Titre.1, font=PoliceTitre)
    tkgrid(button1, row=1, column=2)

    tkgrid(l2, row=2, column=1)
    tkgrid(Titre.2, row=3, column=1, sticky="w")
    tkconfigure(Titre.2, font=PoliceTitre)
    tkgrid(button2, row=3, column=2)

    tkgrid(l3, row=4, column=1)
    tkgrid(Titre.3, row=5, column=1, sticky="w")
    tkconfigure(Titre.3, font=PoliceTitre)
    tkgrid(button3, row=5, column=2)

    tkgrid(l4, row=6, column=1)
    tkgrid(Titre.4, row=7, column=1, sticky="w")
    tkgrid(button4, row=7, column=2)
    tkconfigure(button4,padx=2)
    tkconfigure(Titre.4, font=PoliceTitre)
  }

  .include=function(...)
  {
    load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))

    assign("Inc", tktoplevel(),envir = as.environment(pos))
    tkwm.geometry(Inc, "+550+270")

    assign("mm", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkgrid(mm)
    Titre <- tklabel(mm,text="Treated dose level",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DL <- tclVar(Res$MTD)

    for (i in 1: Study$Nb_Dl)
    {
      tkgrid(assign(paste('rDL',i,sep=''),tkradiobutton(mm,variable=rbValue.DL ,value=i, text=paste(i),font=PoliceGenerale)))
    }

    assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkgrid(Inc.mm1)
    Titre <- tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
    tkgrid(Titre, row=0, columnspan=2)
    rbValue.DLT <- tclVar(0)
    for ( i in 0: 1)
    {
      tkgrid(assign(paste('rDLT',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=i, text=paste(i),font=PoliceGenerale)))
    }
    tkgrid(assign(paste('rDLT',2,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.DLT ,value=2, text=paste("pending"))))

    .Sauv=function(...)
    {

      assign("DL", c(Res$PT.data[,1],as.numeric(tclvalue((rbValue.DL)))),envir = as.environment(pos))
      assign("DLT", c(Res$PT.data[,2],as.numeric(tclvalue((rbValue.DLT)))),envir = as.environment(pos))
      assign("patid", c(Res$PT.data[,3],Res$Pat.included+1),envir = as.environment(pos))
      assign("pndg", as.numeric (c(Res$PT.data[,4],as.numeric(tclvalue((rbValue.DLT)))==2)),envir = as.environment(pos))

      Res$PT.data <- data.frame(DL=DL,DLT=DLT,patid=patid,pndg=pndg)
      Res$Pat.included <- Res$Pat.included+1

      DATA=Res$PT.data[which(Res$PT.data$pndg==0),]

      data.ewoc=DATA[c(3,1,2)]
      names(data.ewoc)=c('patient', 'dose','tox')
      a=dim(data.ewoc)[1]
      if (a>1) {data.ewoc$patient<-c(1:a)}
      if (nrow(DATA)!=0)
      {
        e$EWOC.2<- bcrm::bcrm(stop=list(nmax=1),data=data.ewoc, p.tox0=Study$DL,
                              simulate=FALSE, ff=paste(Study$Model), constrain=Study$Constrain, cohort=1,
                              prior.alpha=list(Study$Prior.c,Study$a,Study$b),target.tox=Study$Target,
                              pointest=Study$Pointest,method="rjags")
        EWOC.2 <- e$EWOC.2
        Res$MTD <- e$EWOC.2$ndose[[1]]$ndose
      }
      if (nrow(DATA)!=0) {
        save(Res,Study,EWOC.2,file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))}
      else {save(Res,Study,file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))}
      load(file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))
      tkdestroy(Inc)
      assign("Res", Res,envir = as.environment(pos))
      assign("Study", Study,envir = as.environment(pos))

      .ReCap()
      .ReCap.Resultat()

      if (Study$Stop==1)
      {
        if(Res$Pat.included==Study$N_Pat)
        {
          .ReCap.Resultat()
          disabled.ReCap()
        }
      }
      if (Study$Stop==2)
      {
        x=Res$PT.data[,1]
        xunique=unique(x)
        for (i in 1 : length(xunique))
        {
          temp=x[which(x==xunique[i])]
          if (length (temp) >= Study$N_Pat)
          {
            .ReCap.Resultat()
            disabled.ReCap()
          }
        }
      }

      if (Res$Pat.included==1)
      {disabled.EWOC()}
    }

    Inc.Win_EWOC<- tkframe(Inc,relief="groove",borderwidth=3)
    Save=tkbutton(Inc.Win_EWOC,text="Validate",width=30,command=.Sauv,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_EWOC)

  }

  Export.Graph=function()
  {
    plot(e$EWOC.2)
  }

  NewEWOC=function()
  {
    tkdestroy(Win_EWOC)
    frameInitEWOC()

    .Crea.f=function(...)
    {
      Nb_Niv_Doses <- as.numeric(tclvalue((SliderValue)))
      for ( i in 1: Nb_Niv_Doses)
      {
        r=17 + i
        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8),envir = as.environment(pos)),row=r,column=1)
      }
      if( Nb_Niv_Doses!=8)
      {
        for ( i in (Nb_Niv_Doses+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }
    }
    .Sauvgarde.Study=function(...)
    {
      Nb_Dl=as.numeric(tclvalue(SliderValue))
      DL=NULL
      if (Nb_Dl==2) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))}
      if (Nb_Dl==3) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))}
      if (Nb_Dl==4) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))}
      if (Nb_Dl==5) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))}
      if (Nb_Dl==6) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))}
      if (Nb_Dl==7) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))}
      if (Nb_Dl==8) {DL=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))}

      Target=as.numeric(tclvalue(tkget((Target))))
      model=tclvalue(tkget((model)))
      if (model=="Hyperbolic tangent")(Model="ht")
      if (model=="Logistic")(Model="logit1")
      if (model=="Power")(Model="power")

      Prior=tclvalue(tkget((Prior.alpha)))
      if (Prior=="Gamma(a,b)")(Prior.c=1)
      if (Prior=="Uniform(a,b)")(Prior.c=2)
      if (Prior=="Lognormal(a,b)")(Prior.c=3)

      a.p=as.numeric(tclvalue(tkget(a)))
      b.p=as.numeric(tclvalue(tkget(b)))
      pointtest=as.numeric(tclvalue(tkget(pointest)))

      Stop=as.numeric(tclvalue(rbValue.SC))
      const=as.logical(as.numeric(tclvalue(rbValue.const)))

      N_Pat=as.numeric(tclvalue(tkget(NBpat)))
      N_pat_Level=as.numeric(tclvalue(tkget(NBpat)))
      assign("names_Study", tclvalue(tkget(names.S)),envir = as.environment(pos))

      if ((Stop == 1) & (N_Pat <=0)) stop('Number of patients to be enrolled in the study <=0')
      if ((Stop == 1) & (N_Pat > 100)) cat("\n Warning: Number of patients to be enrolled in the study > 100")
      if ((Stop == 2) & (N_Pat <=0)) stop('Number of patients by dose level  <=0')
      if ((Stop == 2) & (N_Pat > 20)) cat("\n Warning: Large number of patients by dose level")
      if (any(DL <= 0) | any(DL >= 1)) stop('Prior probabilities must be within ]0,1[ ')
      if (is.unsorted(DL, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
      if (Target>0.55) cat("\n Warning: Target DLT rate too high")
      if (Target<0.15) cat("\n Warning: Target DLT rate too low")
      if ((pointtest < 0) | (pointtest > 0.5)) stop('Pointest parameter must be within [0,0.5]')

      assign("Study", list(name=paste(names_Study),Nb_Dl=Nb_Dl,DL=DL,Target=Target,Model=Model,
                           Stop=Stop,N_Pat=N_Pat,N_pat_Level=N_pat_Level,Prior=Prior,Prior.c=Prior.c,
                           a.p=a.p,b.p=b.p,Pointest=pointtest, Constrain=const), envir = as.environment(pos))

      MTD=max(which(Study$DL==Study$Target),1)
      Pat_include=0
      PT.data=NULL
      assign("Res", list(MTD=MTD,Pat.included=Pat_include,PT.data=PT.data),envir = as.environment(pos))
      save(Res,Study,file =paste(paste(names_Study,'ewoc',sep='-'),'RData',sep='.'))
      .ReCap()

      Study.file=function()
      {
        a00=paste(names_Study)
        a0=paste('Name of study',paste(names_Study),sep=':')
        a1=paste('Method','EWOC',sep=':')
        a2='Escalation With Overdose Control (EWOC) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(names_Study),'ewoc', sep='.'))
      }
      Study.file()
      tk2notetab.select(net.EWOC , "Include")
    }

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=14)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3), envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    assign("m2", tkframe(Required,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    SliderValue=tclVar('8')
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",command=.Crea.f)
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    assign("Target", tkentry(Saisi, width=5),envir = as.environment(pos))
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi) ; rb2 <- tkradiobutton(Saisi)
    rbValue.SC <- tclVar(1)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)
    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding ",font=PoliceGenerale)
    NBpat<-tkentry(Saisi, width=10)
    tkgrid(labage, row=5,column=0, sticky="w", columnspan=2)
    tkgrid(NBpat, row=5,column=2, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    model <-tk2spinbox(Saisi, values = c("Hyperbolic tangent","Logistic","Power"))
    tkgrid(Titre , row=6,column=0, sticky="w")
    tkgrid(model, row=6,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Prior",font=PoliceGenerale)
    Prior.alpha <-tk2spinbox(Saisi, values = c("Gamma(a,b)","Uniform(a,b)","Lognormal(a,b)"))
    tkgrid(Titre , row=7,column=0, sticky="w")
    tkgrid(Prior.alpha , row=7,column=1, sticky="w")

    Titre1=tklabel(Saisi,text="a",font=PoliceGenerale)
    Titre2=tklabel(Saisi,text="b",font=PoliceGenerale)
    assign("a", tkentry(Saisi, width=5),envir = as.environment(pos))
    assign("b", tkentry(Saisi, width=5),envir = as.environment(pos))
    tkgrid(Titre1, row=8, column=0, sticky="w")
    tkgrid(a, row=8, column=1, sticky="w")
    tkgrid(Titre2, row=8, column=2, sticky="w")
    tkgrid(b, row=8, column=3, sticky="w")

    Titre1=tklabel(Saisi,text="Pointest in [0,0.5]",font=PoliceGenerale)
    assign("pointest", tkentry(Saisi, width=5),envir = as.environment(pos))
    tkgrid(Titre1, row=10, column=0, sticky="w")
    tkgrid(pointest, row=10, column=1, sticky="w")

    Titrec=tklabel(Saisi,text="Dose skipping constraint",font=PoliceGenerale)
    const1 <- tkradiobutton(Saisi) ; const2 <- tkradiobutton(Saisi)
    rbValue.const <- tclVar(1)
    tkconfigure(const1,variable=rbValue.const,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2,variable=rbValue.const,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec, row=11, column=0, sticky="w")
    tkgrid(const1, row=11,  column=1, sticky="w")
    tkgrid(const2, row=12,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    names.S<-tkentry(Saisi, width=20)
    tkgrid(labage, row=13,column=0, sticky="w")
    tkgrid(names.S, row=13,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,command=.Sauvgarde.Study,bg="cornflower blue")
    tkgrid(Save.rec, row=1)
  }

  disabled.EWOC=function()
  {
    tkdestroy(Saisi)
    tkdestroy(m2)
    tkdestroy(Saisi.save)
    tkdestroy(Rec.mm0)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=14)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    Nb_Niv_Doses <- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)
    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)
    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w", columnspan=2)
    tkgrid(NBpat, row=5,column=2, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    model <-tk2spinbox(Saisi, values =paste(Study$Model),state="disabled")
    tkgrid(Titre , row=6,column=0, sticky="w")
    tkgrid(model, row=6,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Prior",font=PoliceGenerale)
    Prior.alpha <-tk2spinbox(Saisi, values = paste(Study$Prior),state="disabled")
    tkgrid(Titre , row=7,column=0, sticky="w")
    tkgrid(Prior.alpha , row=7,column=1, sticky="w")

    Titre1=tklabel(Saisi,text="a",font=PoliceGenerale)
    Titre2=tklabel(Saisi,text="b",font=PoliceGenerale)
    a.iter <- tclVar(Study$a.p);b.iter <- tclVar(Study$b.p)
    assign("a", tkentry(Saisi, width=5,textvariable=a.iter ,state="disabled"),envir = as.environment(pos))
    assign("b", tkentry(Saisi, width=5,textvariable=b.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Titre1, row=8, column=0, sticky="w")
    tkgrid(a, row=8, column=1, sticky="w")
    tkgrid(Titre2, row=8, column=2, sticky="w")
    tkgrid(b, row=8, column=3, sticky="w")

    Titre1=tklabel(Saisi,text="Pointest in [0,0.5]",font=PoliceGenerale)
    pointest.iter <- tclVar(Study$Pointest)
    assign("pointest", tkentry(Saisi, width=5,textvariable=pointest.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Titre1, row=10, column=0, sticky="w")
    tkgrid(pointest, row=10, column=1, sticky="w")

    Titrec=tklabel(Saisi,text="Dose skipping constraint",font=PoliceGenerale)
    const1 <- tkradiobutton(Saisi, state="disabled") ; const2 <- tkradiobutton(Saisi, state="disabled")
    rbValue.const <- tclVar(as.numeric(Study$Constrain))
    tkconfigure(const1,variable=rbValue.const,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2,variable=rbValue.const,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec, row=11, column=0, sticky="w")
    tkgrid(const1, row=11,  column=1, sticky="w")
    tkgrid(const2, row=12,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Names of study:",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")

    tkgrid(labage, row=13,column=0, sticky="w")
    tkgrid(names.S, row=13,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,state="disabled",bg="cornflower blue")
    tkgrid(Save.rec, row=1)
    .ReCap()
  }

  Open.EWOC=function()
  {
    tkdestroy(Win_EWOC)
    frameInitEWOC()
    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=10)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)
    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    Nb_Niv_Doses <- Study$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$DL[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,  textvariable=n.iter,state="disabled"),envir = as.environment(pos)) ,sticky="w",row=r,column=1)
    }

    SliderValue=tclVar(Study$Nb_Dl)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=1, sticky="w")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")

    Titre=tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(Study$Target)
    Target<-tkentry(Saisi, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Titre=tklabel(Saisi,text="Stopping rules",font=PoliceGenerale)
    rb1 <- tkradiobutton(Saisi,state="disabled") ; rb2 <- tkradiobutton(Saisi,state="disabled")
    rbValue.SC <- tclVar(Study$Stop)

    tkconfigure(rb1,variable=rbValue.SC,value=1, text="Maximum number of patients to be enrolled in the study",font=PoliceGenerale)
    tkconfigure(rb2,variable=rbValue.SC,value=2, text="Maximum number of patients by dose level",font=PoliceGenerale)

    tkgrid(Titre, row=3, column=0, sticky="w")
    tkgrid(rb1, row=3,  column=1, sticky="w", columnspan=3)
    tkgrid(rb2, row=4,column=1, sticky="w", columnspan=3)

    labage<-tklabel(Saisi, text="Number of patients corresponding",font=PoliceGenerale)
    NP<- tclVar(Study$N_Pat)
    NBpat<-tkentry(Saisi, width=10,textvariable=NP,state="disabled")
    tkgrid(labage, row=5,column=0, sticky="w", columnspan=2)
    tkgrid(NBpat, row=5,column=2, sticky="w")

    Titre=tklabel(Saisi,text="Model",font=PoliceGenerale)
    model <-tk2spinbox(Saisi, values =paste(Study$Model),state="disabled")
    tkgrid(Titre , row=6,column=0, sticky="w")
    tkgrid(model, row=6,column=1, sticky="w")

    Titre=tklabel(Saisi,text="Prior",font=PoliceGenerale)
    Prior.alpha <-tk2spinbox(Saisi, values = paste(Study$Prior),state="disabled")
    tkgrid(Titre , row=7,column=0, sticky="w")
    tkgrid(Prior.alpha , row=7,column=1, sticky="w")

    Titre1=tklabel(Saisi,text="a",font=PoliceGenerale)
    Titre2=tklabel(Saisi,text="b",font=PoliceGenerale)
    a.iter <- tclVar(Study$a.p);b.iter <- tclVar(Study$b.p)
    assign("a", tkentry(Saisi, width=5,textvariable=a.iter ,state="disabled"),envir = as.environment(pos))
    assign("b", tkentry(Saisi, width=5,textvariable=b.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Titre1, row=8, column=0, sticky="w")
    tkgrid(a, row=8, column=1, sticky="w")
    tkgrid(Titre2, row=8, column=2, sticky="w")
    tkgrid(b, row=8, column=3, sticky="w")

    Titre1=tklabel(Saisi,text="Pointest in [0,0.5]",font=PoliceGenerale)
    pointest.iter <- tclVar(Study$Pointest)
    assign("pointest", tkentry(Saisi, width=5,textvariable=pointest.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Titre1, row=10, column=0, sticky="w")
    tkgrid(pointest, row=10, column=1, sticky="w")

    Titrec=tklabel(Saisi,text="Dose skipping constraint",font=PoliceGenerale)
    const1 <- tkradiobutton(Saisi, state="disabled") ; const2 <- tkradiobutton(Saisi, state="disabled")
    rbValue.const <-tclVar(as.numeric(Study$Constrain))
    tkconfigure(const1,variable=rbValue.const,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2,variable=rbValue.const,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec, row=11, column=0, sticky="w")
    tkgrid(const1, row=11,  column=1, sticky="w")
    tkgrid(const2, row=12,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Names of study:",font=PoliceGenerale)
    Nam<- tclVar(Study$name)
    names.S<-tkentry(Saisi, width=20,textvariable=Nam,state="disabled")
    tkgrid(labage, row=13,column=0, sticky="w")
    tkgrid(names.S, row=13,column=1, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=20,state="disabled")
    tkgrid(Save.rec, row=37, column=1, columnspan=2, rowspan=2)
    .ReCap()
    .ReCap.Resultat()
  }


  ExcellExport=function (dframe=NULL) {
    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}

    filef_ <- tclvalue(tkgetSaveFile())
    if (filef_ != "") {
      filesave=paste(filef_,"xlsx",sep=".")
      pdf(paste(filef_,"pdf",sep="."))
      plot(e$ResSim2)
      dev.off()
      wb_ <- createWorkbook()
      sheet1_ <- addWorksheet(wb_, sheetName="Simulation")

      style_ <- createStyle(fontSize = 12, fontColour = "black",
                            textDecoration = c("bold", "italic", "underline"),
                            halign = "center", valign = "center", border = "Bottom",
                            fgFill = "gray")

      setColWidths(wb_, sheet1_, cols=c(1,2,3,4,5), widths = 13.43)

      writeDataTable(wb_,sheet1_,x=dframe)

      addStyle(wb_,sheet1_,style_,cols=1:5,rows=1)

      saveWorkbook(wb_, filesave, overwrite = T)

    }
  }
  .RecapSim=function()
  {

    .Save_sim=function()
    {
      Study.file=function()
      {
        a00=paste(StudyS$Name)
        a0=paste('Name of study',paste(StudyS$Name),sep=':')
        a1=paste('Method','EWOC',sep=':')
        a2='Escalation With Overdose Control (EWOC) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(StudyS$Name),'ewsim', sep='.'))
      }
      Study.file()
      ResSim <- e$ResSim
      save(ResSim,StudyS,file =paste(paste(StudyS$Name,'ewsim',sep='-'),'RData',sep='.'))
      Save.infos <- tkmessageBox(title = "Data storage Infos",
                                 message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)


    Res.print.sim=function() {

      n.average <- mean(sapply(e$ResSim, function(i) {
        dim(i$data)[1]
      }))
      n.min <- min(sapply(e$ResSim, function(i) {
        dim(i$data)[1]
      }))
      n.max <- max(sapply(e$ResSim, function(i) {
        dim(i$data)[1]
      }))

      assign("n.res", cbind(n.average,n.min,n.max),envir = as.environment(pos))

      rec <- sapply(e$ResSim, function(i) {
        i$ndose[[length(i$ndose)]]$ndose
      })
      rec.tab <- prop.table(table(factor(rec, levels = 1:length(e$ResSim[[1]]$tox))))

      exp <- sapply(e$ResSim, function(i) {
        (i$tox + i$notox)/sum(i$tox + i$notox)
      })
      exp.tab <- apply(exp, 1, mean)

      assign("tab.dose", signif(rbind(exp.tab, rec.tab), 3),envir = as.environment(pos))

      tox.cutpoints = NULL
      if (is.null(tox.cutpoints)) {
        tox.cutpoints <- seq(0, 1, by = 0.2)
      }  else {
        tox.cutpoints <- unique(c(0, tox.cutpoints, 1))
      }
      assign("exp.tox", signif(prop.table(table(cut(unlist(sapply(e$ResSim, function(i) {
        rep(i$truep, (i$tox + i$notox))
      }, simplify = FALSE)), tox.cutpoints, include.lowest = T))),2),envir = as.environment(pos))


      tox. <- sapply(e$ResSim, function(i) {
        (i$tox)
      })
      assign("tox.tab", apply(tox., 1, mean),envir = as.environment(pos))
    }

    assign("Rec.fr", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.fr, row=0, column=0, rowspan=20)

    Res.print.sim()
    PriorRes=paste(round(StudyS$Prior[1],3))
    TrueRes=paste(round(StudyS$True[1],3))
    MTDRes=paste(round(tab.dose[2,1],3))
    ToxRes=paste(round(tox.tab[1],3))
    LevelRes=paste(round(tab.dose[1,1],3))
    for( i in 2 : length(StudyS$Prior))
    {
      PriorRes=paste(PriorRes,paste(round(StudyS$Prior[i],3)),sep=", ")
      TrueRes=paste(TrueRes,paste(round(StudyS$True[i],3)),sep=", ")
      MTDRes=paste(MTDRes,paste(round(tab.dose[2,i],3)),sep=", ")
      LevelRes=paste(LevelRes,paste(round(tab.dose[1,i],3)),sep=", ")
      ToxRes=paste(ToxRes,paste(round(tox.tab[i],3)),sep=", ")
    }


    NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")


    Titre1<-tklabel(Rec.fr,text="Results of the simulation")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
    aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
    aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
    aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
    aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
    aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
    aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
    aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
    aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
    aff_1.1<-tklabel(Rec.fr,text=paste("Escalation With Overdose Control "))
    aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
    aff_6.1<-tklabel(Rec.fr,text=paste(ifelse(StudyS$Model=="ht","Hyperbolic tangent",ifelse(StudyS$Model=="logit1","Logistic","Power"))))
    aff_8.0<-tklabel(Rec.fr,text=paste("% recommendation MTD"))
    aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
    aff_7.0<-tklabel(Rec.fr,text=paste("Sample size: mean [min,max]"))
    aff_7.1<-tklabel(Rec.fr,text=paste(paste(paste(n.res[1]),"[",n.res[2]),",",n.res[3],"]"))
    aff_9.0<-tklabel(Rec.fr,text=paste("Percentage of patients treated by dose level"))
    aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
    aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
    aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
    aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
    aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

    tkgrid(aff_1.0, row=4, column=1,  sticky="w")
    tkgrid(aff_1.1, row=4, column=2,  sticky="w")
    tkconfigure(aff_1.0, font=Policeligne)
    tkconfigure(aff_1.1, font=Policeligne , foreground="blue")

    tkgrid(aff_2.0, row=6, column=1, sticky="w")
    tkgrid(aff_2.1, row=6, column=2, sticky="w")
    tkconfigure(aff_2.0, font=Policeligne)
    tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

    tkgrid(aff_3.0, row=7, column=1, sticky="w")
    tkgrid(aff_3.1, row=7, column=2,  sticky="w")
    tkconfigure(aff_3.0, font=Policeligne )
    tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

    tkgrid(aff_4.0, row=8, column=1, sticky="w")
    tkgrid(aff_4.1, row=8, column=2,  sticky="w")
    tkconfigure(aff_4.0, font=Policeligne )
    tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

    tkgrid(aff_5.0, row=9, column=1,  sticky="w")
    tkgrid(aff_5.1, row=9, column=2,sticky="w")
    tkconfigure(aff_5.0, font=Policeligne )
    tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

    tkgrid(aff_6.0, row=10, column=1,  sticky="w")
    tkgrid(aff_6.1, row=10, column=2,  sticky="w")
    tkconfigure(aff_6.0, font=Policeligne)
    tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

    tkgrid(aff_7.0, row=11, column=1,  sticky="w")
    tkgrid(aff_7.1, row=11, column=2,  sticky="w")
    tkconfigure(aff_7.0, font=Policeligne )
    tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

    tkgrid(aff_11.0, row=12, column=1,  sticky="w")
    tkgrid(aff_11.1, row=12, column=2,  sticky="w")
    tkconfigure(aff_11.0, font=Policeligne )
    tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

    tkgrid(aff_8.0, row=13, column=1,  sticky="w")
    tkgrid(aff_8.1, row=13, column=2,  sticky="w")
    tkconfigure(aff_8.0, font=Policeligne )
    tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

    tkgrid(aff_9.0, row=14, column=1,  sticky="w")
    tkgrid(aff_9.1, row=14, column=2,  sticky="w")
    tkconfigure(aff_9.0, font=Policeligne )
    tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

    tkgrid(aff_10.0, row=15, column=1,  sticky="w")
    tkgrid(aff_10.1, row=15, column=2,  sticky="w")
    tkconfigure(aff_10.0, font=Policeligne )
    tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

    assign("Rec.save_", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.save_, row=20, column=0)
    e$ResSim2 <- e$ResSim
    e$ResSim2[[1]]$sdose <- round(e$ResSim[[1]]$sdose,3)
    PlotBut=tkbutton(Rec.save_ ,text="Graphic",width=20,command=function(){plot(e$ResSim2)},bg="cornflower blue")
    tkgrid(PlotBut, row=1)
    l<-tklabel(Rec.save_,text="")
    tkgrid(l, row=2)
    ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,round(tab.dose[2,],3),
                              round(tab.dose[1,],3),round(tox.tab,3)))
    colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.(%)",
                         "(%)patient","Av. tox. ")
    WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                      command=function()ExcellExport(dframe=ResExp_),bg="cornflower blue")
    tkgrid(WordExp_, row=3)
    blank<-tklabel(Rec.save_,text="      ")
    tkgrid(blank, row=4)
    SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,command=.Save_sim,bg="cornflower blue")
    tkgrid(SaveSim_, row=5)
  }

  disabled.SimEWOC=function()
  {

    .RecapSimB=function()
    {

      Res.print.sim = function() {

        n.average <- mean(sapply(e$ResSim, function(i) {
          dim(i$data)[1]
        }))
        n.min <- min(sapply(e$ResSim, function(i) {
          dim(i$data)[1]
        }))
        n.max <- max(sapply(e$ResSim, function(i) {
          dim(i$data)[1]
        }))

        assign("n.res", cbind(n.average,n.min,n.max),envir = as.environment(pos))


        rec <- sapply(e$ResSim, function(i) {
          i$ndose[[length(i$ndose)]]$ndose
        })
        rec.tab <- prop.table(table(factor(rec, levels = 1:length(e$ResSim[[1]]$tox))))

        exp <- sapply(e$ResSim, function(i) {
          (i$tox + i$notox)/sum(i$tox + i$notox)
        })
        exp.tab <- apply(exp, 1, mean)

        assign("tab.dose", signif(rbind(exp.tab, rec.tab), 3),envir = as.environment(pos))

        tox.cutpoints = NULL
        if (is.null(tox.cutpoints)) {
          tox.cutpoints <- seq(0, 1, by = 0.2)
        }  else {
          tox.cutpoints <- unique(c(0, tox.cutpoints, 1))
        }
        assign("exp.tox", signif(prop.table(table(cut(unlist(sapply(e$ResSim, function(i) {
          rep(i$truep, (i$tox + i$notox))
        }, simplify = FALSE)), tox.cutpoints, include.lowest = T))),2),envir = as.environment(pos))

        tox. <- sapply(e$ResSim, function(i) {
          (i$tox)
        })
        assign("tox.tab", apply(tox., 1, mean),envir = as.environment(pos))
      }

      PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
      PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

      assign("Rec.fr", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=20)

      Res.print.sim()
      PriorRes=paste(round(StudyS$Prior[1],3))
      TrueRes=paste(round(StudyS$True[1],3))
      MTDRes=paste(round(tab.dose[2,1],3))
      ToxRes=paste(round(tox.tab[1],3))
      LevelRes=paste(round(tab.dose[1,1],3))
      for( i in 2 : length(StudyS$Prior))
      {
        PriorRes=paste(PriorRes,paste(round(StudyS$Prior[i],3)),sep=", ")
        TrueRes=paste(TrueRes,paste(round(StudyS$True[i],3)),sep=", ")
        MTDRes=paste(MTDRes,paste(round(tab.dose[2,i],3)),sep=", ")
        LevelRes=paste(LevelRes,paste(round(tab.dose[1,i],3)),sep=", ")
        ToxRes=paste(ToxRes,paste(round(tox.tab[i],3)),sep=", ")
      }

      NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
      tkgrid(NS, row=1, column=1,  sticky="w")
      tkconfigure(NS, font=PoliceEtude, foreground="white")

      Titre1<-tklabel(Rec.fr,text="Results of the simulation")
      tkgrid(Titre1, row=3, column=1)
      tkconfigure(Titre1, font=PoliceTitre)

      aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
      aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
      aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
      aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
      aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
      aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
      aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
      aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
      aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
      aff_1.1<-tklabel(Rec.fr,text=paste("Escalation With Overdose Control "))
      aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
      aff_6.1<-tklabel(Rec.fr,text=paste(ifelse(StudyS$Model=="ht","Hyperbolic tangent",ifelse(StudyS$Model=="logit1","Logistic","Power"))))
      aff_8.0<-tklabel(Rec.fr,text=paste("% recommendation MTD"))
      aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
      aff_7.0<-tklabel(Rec.fr,text=paste("Sample size: mean [min,max]"))
      aff_7.1<-tklabel(Rec.fr,text=paste(paste(paste(n.res[1]),"[",n.res[2]),",",n.res[3],"]"))
      aff_9.0<-tklabel(Rec.fr,text=paste("Percentage of patients treated by dose level"))
      aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
      aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
      aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
      aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
      aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))

      tkgrid(aff_1.0, row=4, column=1,  sticky="w")
      tkgrid(aff_1.1, row=4, column=2,  sticky="w")
      tkconfigure(aff_1.0, font=Policeligne)
      tkconfigure(aff_1.1, font=Policeligne , foreground="blue")

      tkgrid(aff_2.0, row=6, column=1, sticky="w")
      tkgrid(aff_2.1, row=6, column=2, sticky="w")
      tkconfigure(aff_2.0, font=Policeligne)
      tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

      tkgrid(aff_3.0, row=7, column=1, sticky="w")
      tkgrid(aff_3.1, row=7, column=2,  sticky="w")
      tkconfigure(aff_3.0, font=Policeligne )
      tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

      tkgrid(aff_4.0, row=8, column=1, sticky="w")
      tkgrid(aff_4.1, row=8, column=2,  sticky="w")
      tkconfigure(aff_4.0, font=Policeligne )
      tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

      tkgrid(aff_5.0, row=9, column=1,  sticky="w")
      tkgrid(aff_5.1, row=9, column=2,sticky="w")
      tkconfigure(aff_5.0, font=Policeligne )
      tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

      tkgrid(aff_6.0, row=10, column=1,  sticky="w")
      tkgrid(aff_6.1, row=10, column=2,  sticky="w")
      tkconfigure(aff_6.0, font=Policeligne)
      tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

      tkgrid(aff_7.0, row=11, column=1,  sticky="w")
      tkgrid(aff_7.1, row=11, column=2,  sticky="w")
      tkconfigure(aff_7.0, font=Policeligne )
      tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

      tkgrid(aff_11.0, row=12, column=1,  sticky="w")
      tkgrid(aff_11.1, row=12, column=2,  sticky="w")
      tkconfigure(aff_11.0, font=Policeligne )
      tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

      tkgrid(aff_8.0, row=13, column=1,  sticky="w")
      tkgrid(aff_8.1, row=13, column=2,  sticky="w")
      tkconfigure(aff_8.0, font=Policeligne )
      tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

      tkgrid(aff_9.0, row=14, column=1,  sticky="w")
      tkgrid(aff_9.1, row=14, column=2,  sticky="w")
      tkconfigure(aff_9.0, font=Policeligne )
      tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

      tkgrid(aff_10.0, row=15, column=1,  sticky="w")
      tkgrid(aff_10.1, row=15, column=2,  sticky="w")
      tkconfigure(aff_10.0, font=Policeligne )
      tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

      assign("Rec.save_", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
      tkgrid(Rec.save_, row=20, column=0)
      e$ResSim2 <- e$ResSim
      e$ResSim2[[1]]$sdose<-round(e$ResSim[[1]]$sdose,3)
      PlotBut=tkbutton(Rec.save_ ,text="Graphic",width=20,command=function(){plot(e$ResSim2)},bg="cornflower blue")
      tkgrid(PlotBut, row=1)
      l<-tklabel(Rec.save_,text="")
      tkgrid(l, row=2)
      ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,round(tab.dose[2,],3),
                                round(tab.dose[1,],3),round(tox.tab,3)))
      colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.(%)",
                           "(%)patient","Av. tox. ")
      WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                        command=function()ExcellExport(dframe=ResExp_),bg="cornflower blue")
      tkgrid(WordExp_, row=3)
      blank<-tklabel(Rec.save_,text="      ")
      tkgrid(blank, row=4)

      SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,state="disabled",bg="cornflower blue")
      tkgrid(SaveSim_, row=5)
    }

    tkdestroy(Saisi_)
    tkdestroy(m2_)
    tkdestroy(Run_)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")


    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=8)


    Nb_Niv_Doses <- StudyS$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (StudyS$Prior[i])
      n.itert<- tclVar (StudyS$True[i])


      tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
      tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8, textvariable=n.itert,state="disabled"),envir = as.environment(pos)),row=r,column=1)

      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=3)

    }


    SliderValue_=tclVar(StudyS$Nb_Dl)
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",state="disabled")

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0,columnspan=4, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=3, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Target)
    Target_<-tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled")

    tkgrid(Titre_, row=2, column=0,columnspan=3, sticky="w")
    tkgrid(Target_, row=2, column=3, sticky="w")


    l<-tklabel(Saisi_,text="            ")
    tkgrid(l, row=2, column=3)


    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Nsim)
    Nsim_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tnsim_, row=4,column=0,columnspan=3, sticky="w")
    tkgrid(Nsim_, row=4,column=3, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$X0)
    X0_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(TX0_, row=5,column=0,columnspan=3, sticky="w")
    tkgrid(X0_, row=5,column=3, sticky="w")


    if (StudyS$Model=="ht")(model="Hyperbolic tangent")
    if (StudyS$Model=="logit1")(model="Logistic")
    if (StudyS$Model=="power")(model="Power")

    if (StudyS$Prior.a==1)(prior.a="Gamma(a,b)")
    if (StudyS$Prior.a==2)(prior.a="Uniform(a,b)")
    if (StudyS$Prior.a==3)(prior.a="Lognormal(a,b)")

    Tmod_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    Model_ <-tk2spinbox(Saisi_, values = paste(model),state="disabled")
    tkgrid(Tmod_ , row=6,column=0, sticky="w")
    tkgrid(Model_, row=6,column=2,columnspan=3, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Seed)
    Seed_<-tkentry(Saisi_, width=10,textvariable=n.iter ,state="disabled")
    tkgrid(Tseed_, row=7,column=0,columnspan=4, sticky="w")
    tkgrid(Seed_, row=7,column=4, sticky="w")


    Tstop_=tklabel(Saisi_,text="Stopping rule",font=PoliceGenerale)
    rbstop.1_ <- tkradiobutton(Saisi_,state="disabled") ; rbstop.2_ <- tkradiobutton(Saisi_,state="disabled")
    if (!is.null(StudyS$Nmax)){rbstop.M_ <- tclVar(1)}
    if (!is.null(StudyS$Nmtd)){rbstop.M_ <- tclVar(2)}
    tkconfigure(rbstop.1_,variable=rbstop.M_,value=1, text="Maximum sample size",font=PoliceGenerale)
    tkconfigure(rbstop.2_,variable=rbstop.M_,value=2, text="Maximum number of patients treated at final MTD",font=PoliceGenerale)

    tkgrid(Tstop_, row=8,column=0,columnspan=3, sticky="w")
    tkgrid(rbstop.1_, row=8,column=3,columnspan=3, sticky="w")
    tkgrid(rbstop.2_, row=9,column=3,columnspan=5, sticky="w")


    Tnum_=tklabel(Saisi_,text="Number of patients corresponding",font=PoliceGenerale)
    if (!is.null(StudyS$Nmax)){n.iter <- tclVar(StudyS$Nmax)}
    if (!is.null(StudyS$Nmtd)){n.iter <- tclVar(StudyS$Nmtd)}
    assign("N_", tkentry(Saisi_,width=5,textvariable=n.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Tnum_, row=10, column=0,columnspan=4, sticky="w")
    tkgrid(N_, row=10, column=4, sticky="w")

    Tpriora_=tklabel(Saisi_,text="Prior Alpha",font=PoliceGenerale)
    Prior.alpha_ <-tk2spinbox(Saisi_, values =paste(prior.a),state="disabled")
    tkgrid(Tpriora_ , row=11,column=0,columnspan=2, sticky="w")
    tkgrid(Prior.alpha_ , row=11,column=2,columnspan=3, sticky="w")

    Ta_=tklabel(Saisi_,text="a",font=PoliceGenerale)
    Tb_=tklabel(Saisi_,text="b",font=PoliceGenerale)
    na <- tclVar(StudyS$a)
    nb <- tclVar(StudyS$b)
    assign("a_", tkentry(Saisi_, width=5,textvariable=na ,state="disabled"),envir = as.environment(pos))
    assign("b_", tkentry(Saisi_, width=5,textvariable=nb ,state="disabled"),envir = as.environment(pos))
    tkgrid(Ta_, row=12, column=0, sticky="w")
    tkgrid(a_, row=12, column=1, sticky="w")
    tkgrid(Tb_, row=12, column=2, sticky="w")
    tkgrid(b_, row=12, column=3, sticky="w")

    Tpoint_=tklabel(Saisi_,text="Pointest in [0,0.5]",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Pointest)
    assign("pointest_", tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled"),envir = as.environment(pos))
    tkgrid(Tpoint_, row=13, column=0,columnspan=2, sticky="w")
    tkgrid(pointest_, row=13, column=2, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_, state="disabled") ; const2_ <- tkradiobutton(Saisi_, state="disabled")
    rbValue.const_ <- tclVar(as.numeric(StudyS$Constrain))
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=14, column=0, sticky="w")
    tkgrid(const1_, row=14,  column=1, sticky="w")
    tkgrid(const2_, row=15,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(StudyS$Name)
    names.S_<-tkentry(Saisi_, width=20,textvariable=Nam_,state="disabled")
    tkgrid(labage_, row=16,column=0,columnspan=2, sticky="w")
    tkgrid(names.S_, row=16,column=2,columnspan=2, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,state="disabled",bg="cornflower blue")
    tkgrid(RunSim_, row=1)

    .RecapSimB()
  }

  SimEWOC=function()
  {

    tkdestroy(Win_EWOC)
    frameInitSIM()

    .Crea.f_=function(...)
    {
      Nb_Niv_Doses_<-as.numeric(tclvalue((SliderValue_)))
      assign("m2_", tkframe(Required_,relief="groove",borderwidth=2),envir = as.environment(pos))
      tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)

      for ( i in 1: Nb_Niv_Doses_)
      {
        r=17 + i
        tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=1)

        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=3)
      }
      if( Nb_Niv_Doses_!=8)
      {
        for ( i in (Nb_Niv_Doses_+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)

          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=2)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=3)
        }
      }
    }

    .Simulation_=function (...)
    {
      .Sauvgarde.Study_=function(...)
      {
        Nb_Dl=as.numeric(tclvalue(SliderValue_))
        prior=NULL
        true=NULL
        if (Nb_Dl==2) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))))}
        if (Nb_Dl==3) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))))}
        if (Nb_Dl==4) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))))}
        if (Nb_Dl==5) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))))}
        if (Nb_Dl==6) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))))}
        if (Nb_Dl==7) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))))}
        if (Nb_Dl==8) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))
        true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))),as.numeric(tclvalue(tkget(valt8))))}


        seed=1
        target=as.numeric(tclvalue(tkget((Target_))))
        nmax=NULL
        nmtd=NULL

        Stopr=as.numeric(tclvalue((rbstop.M_)))
        Const=as.logical(as.numeric(tclvalue((rbValue.const_))))

        Model=tclvalue(tkget((Model_)))
        if (Model=="Hyperbolic tangent")(model="ht")
        if (Model=="Logistic")(model="logit1")
        if (Model=="Power")(model="power")

        Prior=tclvalue(tkget((Prior.alpha_)))
        if (Prior=="Gamma(a,b)")(Prior.a=1)
        if (Prior=="Uniform(a,b)")(Prior.a=2)
        if (Prior=="Lognormal(a,b)")(Prior.a=3)

        a.p=as.numeric(tclvalue(tkget(a_)))
        b.p=as.numeric(tclvalue(tkget(b_)))
        pointest=as.numeric(tclvalue(tkget(pointest_)))


        nsim=as.numeric(tclvalue(tkget((Nsim_))))
        x0=as.numeric(tclvalue(tkget((X0_))))
        seed=as.numeric(tclvalue(tkget((Seed_))))
        assign("Names_Study", tclvalue(tkget(names.S_)),envir = as.environment(pos))
        if (Stopr == 1) {nmax=as.numeric(tclvalue(tkget(N_)))}
        if (Stopr == 2) {nmtd=as.numeric(tclvalue(tkget(N_)))}
        res.users <- 1
        if (target>0.55) cat("\n Warning: Target DLT rate too high")
        if (target<0.15) cat("\n Warning: Target DLT rate too low")
        if (Stopr == 1) {
          if (nmax <=0) stop('Number of patients to be enrolled <=0')
          if (nmax > 100) cat("\n Warning: Number of patients to be enrolled > 100")
        }
        if (Stopr == 2) {
          if (nmtd <= 0) stop('Number of patients treated at final MTD <=0')
          if (nmtd > 20) cat("\n Warning: Number of patients treated at final MTD > 20")
        }
        if ((x0 <1)|(x0 > Nb_Dl)|(x0%%1 != 0)) stop(paste('Starting dose level incorrect, enter an integer between 1 and',Nb_Dl))
        if ((pointest < 0) | (pointest > 0.5)) stop('Pointest parameter must be within [0,0.5]')

        if (nsim < 100) cat("\n Warning: Low number of simulations")
        check_nsim <- function (){
          res.users <- 2
          while (res.users != 0 & res.users != 1) {
            cat("\n Warning: Large number of simulations, continue?  y or n")
            yn <- readLines(n=1)
            if(yn == "y" | yn == "Y" | yn== "yes" | yn == "Yes"){res.users <- 1;}
            if(yn == "n" | yn == "N" | yn== "no" | yn == "No"){res.users <- 0;}
          }
          return(res.users)
        }

        if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
        if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
        if (any(true <= 0) | any(true >= 1)) stop('True probabilities must be within ]0,1[ ')
        if (is.unsorted(true, strictly = TRUE)) stop('True probabilities should be monotonically increasing')
        if (nsim > 10000)  {res.users <- check_nsim()}


        assign("StudyS", list(Name=paste(Names_Study),Nb_Dl=Nb_Dl,True=true,Prior=prior,Target=target,
                              Model=model,Prior.a=Prior.a,a=a.p,b=b.p,Nmax=nmax,Nmtd=nmtd,Pointest=pointest,
                              Nsim=nsim,X0=x0,Seed=seed, Constrain=Const),envir = as.environment(pos))

        if (res.users == 1) { cat("\n Submission in progress... Please wait... ", "\n")
          e$ResSim<- bcrm::bcrm(stop=list(nmax=StudyS$Nmax,nmtd=StudyS$Nmtd),p.tox0=StudyS$Prior,
                                truep=StudyS$True,ff=StudyS$Model, target.tox=StudyS$Target, cohort=1,
                                prior.alpha=list(StudyS$Prior.a,StudyS$a,StudyS$b), pointest=StudyS$Pointest,
                                start=StudyS$X0,simulate=TRUE,nsims=StudyS$Nsim,method="rjags",quietly=TRUE,
                                constrain=StudyS$Constrain, seed=StudyS$Seed)

          .RecapSim()
          tk2notetab.select(net.EWOC_, "Results")
        }

      }
      .Sauvgarde.Study_()


    }


    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")



    SliderValue_=tclVar('8')

    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",command=.Crea.f_)

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0,columnspan=4, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=3, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    assign("Target_", tkentry(Saisi_, width=5),envir = as.environment(pos))

    tkgrid(Titre_, row=2, column=0,columnspan=3, sticky="w")
    tkgrid(Target_, row=2, column=3, sticky="w")

    l<-tklabel(Saisi_,text="            ")
    tkgrid(l, row=2, column=3)

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    Nsim_<-tkentry(Saisi_, width=10)
    tkgrid(Tnsim_, row=4,column=0,columnspan=3, sticky="w")
    tkgrid(Nsim_, row=4,column=3, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    X0_<-tkentry(Saisi_, width=10)
    tkgrid(TX0_, row=5,column=0,columnspan=3, sticky="w")
    tkgrid(X0_, row=5,column=3, sticky="w")

    Tmod_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    Model_ <-tk2spinbox(Saisi_, values = c("Hyperbolic tangent","Logistic","Power"))
    tkgrid(Tmod_ , row=6,column=0, sticky="w")
    tkgrid(Model_, row=6,column=2,columnspan=3, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    Seed_<-tkentry(Saisi_, width=10)
    tkgrid(Tseed_, row=7,column=0,columnspan=4, sticky="w")
    tkgrid(Seed_, row=7,column=4, sticky="w")

    Tstop_=tklabel(Saisi_,text="Stopping rule",font=PoliceGenerale)
    rbstop.1_ <- tkradiobutton(Saisi_) ; rbstop.2_ <- tkradiobutton(Saisi_)
    rbstop.M_ <- tclVar(1)
    tkconfigure(rbstop.1_,variable=rbstop.M_,value=1, text="Maximum sample size",font=PoliceGenerale)
    tkconfigure(rbstop.2_,variable=rbstop.M_,value=2, text="Maximum number of patients treated at final MTD",font=PoliceGenerale)

    tkgrid(Tstop_, row=8,column=0,columnspan=3, sticky="w")
    tkgrid(rbstop.1_, row=8,column=3,columnspan=3, sticky="w")
    tkgrid(rbstop.2_, row=9,column=3,columnspan=5, sticky="w")


    Tnum_=tklabel(Saisi_,text="Number of patients corresponding",font=PoliceGenerale)
    assign("N_", tkentry(Saisi_, width=5),envir = as.environment(pos))
    tkgrid(Tnum_, row=10, column=0,columnspan=4, sticky="w")
    tkgrid(N_, row=10, column=4, sticky="w")

    Tpriora_=tklabel(Saisi_,text="Prior Alpha",font=PoliceGenerale)
    Prior.alpha_ <-tk2spinbox(Saisi_, values = c("Gamma(a,b)","Uniform(a,b)","Lognormal(a,b)"))
    tkgrid(Tpriora_ , row=11,column=0,columnspan=2, sticky="w")
    tkgrid(Prior.alpha_ , row=11,column=2,columnspan=3, sticky="w")

    Ta_=tklabel(Saisi_,text="a",font=PoliceGenerale)
    Tb_=tklabel(Saisi_,text="b",font=PoliceGenerale)
    assign("a_", tkentry(Saisi_, width=5),envir = as.environment(pos));
    assign("b_", tkentry(Saisi_, width=5),envir = as.environment(pos))
    tkgrid(Ta_, row=12, column=0, sticky="w")
    tkgrid(a_, row=12, column=1, sticky="w")
    tkgrid(Tb_, row=12, column=2, sticky="w")
    tkgrid(b_, row=12, column=3, sticky="w")

    Tpoint_=tklabel(Saisi_,text="Pointest in [0,0.5]",font=PoliceGenerale)
    assign("pointest_", tkentry(Saisi_, width=5),envir = as.environment(pos))
    tkgrid(Tpoint_, row=13, column=0,columnspan=2, sticky="w")
    tkgrid(pointest_, row=13, column=2, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_) ; const2_ <- tkradiobutton(Saisi_)
    rbValue.const_ <- tclVar(1)
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=14, column=0, sticky="w")
    tkgrid(const1_, row=14,  column=1, sticky="w")
    tkgrid(const2_, row=15,column=1, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    names.S_<-tkentry(Saisi_, width=20)

    tkgrid(labage_, row=16,column=0,columnspan=2, sticky="w")
    tkgrid(names.S_, row=16,column=2,columnspan=2, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,command=.Simulation_,bg="cornflower blue")
    tkgrid(RunSim_, row=1)

  }


  save_Qf2=function(){ tkmessageBox(title = "Data storage Infos",
                                    message = "Data are automatically saved after each inclusion", icon = "info", type = "ok")}
  save_Qf=function(){ tkmessageBox(title = "Data storage Infos",
                                   message = "Please use the button save study available in interactive EWOC or simulator to save your data", icon = "info", type = "ok")}


  about=function(){ tkmessageBox(title = "Information",
                                 message = "Escalade With Overdose Control Interface 2017",
                                 icon = "info", type = "ok")}

  Open.help=function() {browseURL("https://cran.r-project.org/package=bcrm", browser=getOption("browser"),
                                  encodeIfNeeded = FALSE) }





  frameInit=function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_EWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_EWOC, "630x550")
    tkwm.geometry(Win_EWOC, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_EWOC, ilogo)
    tktitle(Win_EWOC) <- "GUI EWOC"
    tkpack.propagate(Win_EWOC, FALSE)

    topMenu <- tk2menu(Win_EWOC)
    tkconfigure(Win_EWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive EWOC", command=NewEWOC)
    tkadd(newStudyMenu,"command", label="EWOC simulator", command=SimEWOC)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_EWOC); frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_EWOC))
  }

  frameInitEWOC=function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_EWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_EWOC, "630x610")
    tkwm.geometry(Win_EWOC, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_EWOC, ilogo)
    tktitle(Win_EWOC) <- "GUI EWOC"
    tkpack.propagate(Win_EWOC, FALSE)
    names_Study=''

    topMenu <- tk2menu(Win_EWOC)
    tkconfigure(Win_EWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive EWOC", command=NewEWOC)
    tkadd(newStudyMenu,"command", label="EWOC simulator", command=SimEWOC)
    tkadd(StudyMenu,"command", label="Open Study",command=loads)

    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_EWOC); frameInit()} )
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf2)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_EWOC))

    assign("net.EWOC", tk2notebook(Win_EWOC, tabs = c("Input parameters","Include","Results")),envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.EWOC , fill = "both", expand = 1)

    assign("include", tk2notetab(net.EWOC , "Include"),envir = as.environment(pos))
    tkpack.propagate(include, FALSE)

    assign("Resultas", tk2notetab(net.EWOC , "Results"),envir = as.environment(pos))
    tkpack.propagate(Resultas, FALSE)

    assign("Required", tk2notetab(net.EWOC , "Input parameters"),envir = as.environment(pos))
    tkpack.propagate(Required, FALSE)

    assign("Saisi", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)

    assign("Saisi.save", tkframe(Required,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)

    assign("m2", tkframe(Required,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)

    assign("Rec.Win_EWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.Win_EWOC, row=9, column=2, rowspan=16)
    assign("Rec.mm0", tkframe(include, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, columnspan=3, rowspan=16)

    assign("halfwidthPrior", NULL,envir = as.environment(pos))
    assign("nuPrior", NULL,envir = as.environment(pos))
    assign("etatPrior", FALSE,envir = as.environment(pos))
  }

  frameInitSIM=function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_EWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_EWOC, "765x700")
    tkwm.geometry(Win_EWOC, "+460+100")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_EWOC, ilogo)
    tktitle(Win_EWOC) <- "GUI EWOC"
    tkpack.propagate(Win_EWOC, FALSE)
    names_Study=''

    topMenu <- tk2menu(Win_EWOC)
    tkconfigure(Win_EWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive EWOC", command=NewEWOC)
    tkadd(newStudyMenu,"command", label="EWOC simulator", command=SimEWOC)
    tkadd(StudyMenu,"command", label="Open Study",command=loads)

    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_EWOC); frameInit()})
    tkadd(AideMenu,"command", label="Help R (manual)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_EWOC))

    assign("net.EWOC_", tk2notebook(Win_EWOC, tabs = c("Input parameters","Results")),envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.EWOC_ , fill = "both", expand = 1)

    assign("Resultas_", tk2notetab(net.EWOC_ , "Results"),envir = as.environment(pos))
    tkpack.propagate(Resultas_, FALSE)

    assign("Required_", tk2notetab(net.EWOC_ , "Input parameters"),envir = as.environment(pos))
    tkpack.propagate(Required_, FALSE)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2_, row=17, column=0, columnspan=2, rowspan=7)
  }

  frameInit()
}


.TEWOC=function(pos=1, e)
{
  e$TEWOC. = NULL
  e$ResSim = NULL
  tclRequire("Tktable")
  loads <- function()
  {

    loads.infos <- tkmessageBox(title = "Data loading Infos",
                                message = "Select a file.tewoc or file.tewsim", icon = "info", type = "ok")

    filef <- tclvalue(tkgetOpenFile())
    if (filef != "") {
      stu=read.table(filef,header=TRUE)
      assign("Names_Study", paste(stu[1,]),envir = as.environment(pos))
      extension<-str_sub(filef,-6)
    }
    if (extension == ".tewoc") {
      load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      assign("Res", Res,envir = as.environment(pos))
      assign("Study", Study,envir = as.environment(pos))
      e$TEWOC. <- TEWOC.
      disabled.TEWOC()
      .ReCap.Resultat()
      if (Res$Pat.included >= Study$Npat)
      { cat('Maximal number of patients reached out', "\n")
        disabled.Recap()
      }else { .ReCap() }
    }

    if (extension == "tewsim") {
      load(file =paste(paste(Names_Study,'tewsim',sep='-'),'RData',sep='.'),envir = as.environment(pos))
      e$ResSim <- ResSim
      assign("StudyS", StudyS,envir = as.environment(pos))
      tkdestroy(Win_TEWOC)
      frameInitSIM()
      disabled.SimTEWOC()
    }

  }

  ExcellExport=function (dframe=NULL) {
    if (!is.data.frame(dframe)) {stop("The argument must be a data.frame object")}

    filef_ <- tclvalue(tkgetSaveFile())
    if (filef_ != "") {
      filesave=paste(filef_,"xlsx",sep=".")

      wb_ <- createWorkbook()
      sheet1_ <- addWorksheet(wb_, sheetName="Simulation")

      style_ <- createStyle(fontSize = 12, fontColour = "black",
                            textDecoration = c("bold", "italic", "underline"),
                            halign = "center", valign = "center", border = "Bottom",
                            fgFill = "gray")

      setColWidths(wb_, sheet1_, cols=c(1,2,3,4,5), widths = 13.43)

      writeDataTable(wb_,sheet1_,x=dframe)

      addStyle(wb_,sheet1_,style_,cols=1:5,rows=1)

      saveWorkbook(wb_, filesave, overwrite = T)

    }
  }
  .include=function(...)
  {
    load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'))

    assign("mess", tkmessageBox(message = "Do you want to enter mutiple patient data at the same time?",
                                icon = "question", type = "yesno", default = "yes"),envir = as.environment(pos))

    if (tclvalue(mess)=="no") {
      assign("Inc", tktoplevel(),envir = as.environment(pos))
      tkwm.geometry(Inc, "+550+270")

      assign("mm", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
      tkgrid(mm)
      Titre <- tklabel(mm,text="Treated dose level",font=PoliceGenerale)
      tkgrid(Titre, row=0, columnspan=2)
      rbValue.DL <- tclVar(Res$MTD)

      for (i in 1: Study$Nb_Dl)
      {
        tkgrid(assign(paste('rDL',i,sep=''),tkradiobutton(mm,variable=rbValue.DL ,value=i, text=paste(i),font=PoliceGenerale)))
      }

      assign("Inc.mm1", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
      tkgrid(Inc.mm1)
      Titre <- tklabel(Inc.mm1,text="DLT",font=PoliceGenerale)
      tkgrid(Titre, row=0, columnspan=2)
      rbValue.tox <- tclVar(0)
      for ( i in 0: 1)
      {
        tkgrid(assign(paste('rtox',i,sep=''),tkradiobutton(Inc.mm1,variable=rbValue.tox ,value=i, text=paste(i),font=PoliceGenerale)))
      }

      assign("Fup", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
      tkgrid(Fup)

      TitreFup <- tklabel(Fup,text="Follow-up Time")
      tkgrid(TitreFup, row=0, columnspan=2)

      val = tclVar(Study$Obs.wind)
      assign("Followup", tkentry(Fup,textvariable=val, width=10),envir = as.environment(pos))
      tkgrid(Followup)
    } else if (tclvalue(mess)=="yes") {
      Police <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      assign("Inc", tktoplevel(),envir = as.environment(pos))
      tkwm.geometry(Inc, "+550+270")
      tkwm.geometry(Inc, "505x230")

      assign("mm", tkframe(Inc,relief="groove",borderwidth=3),envir = as.environment(pos))
      tkgrid(mm)
      Rules <- tklabel(mm,text="The data must be separated with comma (ex: 1,1,2)",font=Police)
      tkgrid(Rules, row=0, columnspan=2)

      space0 <- tklabel(mm, text=" ")
      tkgrid(space0, row=1, column=0)

      Titre <- tklabel(mm,text="Treated dose level",font=PoliceGenerale)
      tkgrid(Titre, row=2, column=0)
      level. <- tkentry(mm, width=60)
      tkgrid(level., row=2, column=1)

      space1 <- tklabel(mm, text=" ")
      tkgrid(space1, row=3, column=0)

      Ttox <- tklabel(mm,text="DLT (0=No, 1=Yes)",font=PoliceGenerale)
      tkgrid(Ttox, row=4, column=0)
      tox. <- tkentry(mm, width=60)
      tkgrid(tox., row=4, column=1)

      space2 <- tklabel(mm, text=" ")
      tkgrid(space2, row=5, column=0)

      TFup <- tklabel(mm,text=paste("Follow-up","<=",Study$Obs.wind,sep=' '),
                      font=PoliceGenerale)
      tkgrid(TFup, row=6, column=0)
      Fup. <- tkentry(mm, width=60)
      tkgrid(Fup., row=6, column=1)
    }

    .Sauv=function(...)
    {
      if (tclvalue(mess)=="yes") {
        assign("DL", c(Res$PT.data[,1],as.numeric(c(do.call("cbind",strsplit(tclvalue(tkget(level.)),","))))),envir = as.environment(pos))
        assign("tox", c(Res$PT.data[,2],as.numeric(c(do.call("cbind",strsplit(tclvalue(tkget(tox.)),","))))),envir = as.environment(pos))
        assign("fup",c(Res$PT.data[,4],as.numeric(c(do.call("cbind",strsplit(tclvalue(tkget(Fup.)),","))))),envir = as.environment(pos))
        assign("patid", c(Res$PT.data[,3],c((Res$Pat.included+1):(length(DL)))),envir = as.environment(pos))
        Res$Pat.included=length(patid)
      } else {
        assign("DL", c(Res$PT.data[,1],as.numeric(tclvalue(rbValue.DL))),envir = as.environment(pos))
        assign("tox", c(Res$PT.data[,2],as.numeric(tclvalue(rbValue.tox))),envir = as.environment(pos))
        Res$Pat.included<-Res$Pat.included+1
        assign("patid", c(Res$PT.data[,3],Res$Pat.included),envir = as.environment(pos))
        assign("fup", c(Res$PT.data[,4],as.numeric(tclvalue(tkget(Followup)))),envir = as.environment(pos))
      }

      Res$PT.data <- data.frame(DL=DL,Tox=tox,patid=patid,Fup=fup)

      e$TEWOC. <- .titeewoc(prior=Study$Prior, target=Study$Target, tox=Res$PT.data[,2],
                            level=Res$PT.data[,1], followup=Res$PT.data[,4], obswin=Study$Obs.wind,
                            pointest=Study$Pointest, method="rjags", model=Study$Model, intcpt=Study$Intcpt,
                            p.dist=Study$P.dist, p1=Study$P1, p2=Study$P2, alphap=Study$Alphap, constrain= Study$Constrain)

      Res$MTD <- e$TEWOC.$dose.rec
      TEWOC. <- e$TEWOC.
      save(Res,Study,TEWOC.,file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'))
      load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'))
      tkdestroy(Inc)
      assign("Res", Res,envir = as.environment(pos))
      assign("Study", Study,envir = as.environment(pos))
      .ReCap()
      .ReCap.Resultat()

      if(Res$Pat.included==Study$Npat)
      {
        disabled.Recap()
      }
      if(Res$Pat.included > Study$Npat)
      {
        disabled.Recap()
        Save.infos <- tkmessageBox(title = "Stopping rule",
                                   message = "More patients than expected were included",
                                   icon = "warning", type = "ok")

      }

    }

    Inc.Win_EWOC<- tkframe(Inc,relief="groove",borderwidth=3)
    Save=tkbutton(Inc.Win_EWOC,text="Validate",width=10,command=.Sauv,bg="cornflower blue")
    tkgrid(Save)
    tkgrid(Inc.Win_EWOC)

  }


  .ReCap=function(...)
  {

    load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    priorp=paste(Study$Prior[1])
    for( i in 2 : length(Study$Prior))
    {
      priorp=paste(priorp,paste(Study$Prior[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(include, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(priorp))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Time-to-Event Escalation With Overdose Control (Tite-EWOC)"))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model=="ht","Hyperbolic tangent",ifelse(Study$Model=="logit1","Logistic","Power"))))
    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste("Sample size"))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$Npat))

    affiche_10.0<-tklabel(Rec.mm0,text=paste("Observation window "))
    affiche_10.1<-tklabel(Rec.mm0,text=paste(Study$Obs.wind))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")


    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_10.0, row=11, column=1,  sticky="w")
    tkgrid(affiche_10.1, row=11, column=2,  sticky="w")
    tkconfigure(affiche_10.0, font=Policeligne )
    tkconfigure(affiche_10.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="red")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="red")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_TEWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
    Save=tkbutton(Rec.Win_TEWOC,text="New patient",width=30,command=.include,bg="cornflower blue")
    Modif=tkbutton(Rec.Win_TEWOC,text="Modif patient data",width=30,command=.modif.data.patient,bg="cornflower blue")
    tkgrid(Rec.Win_TEWOC, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    tkgrid(Modif, row=0, column=1)

  }

  .modif.data.patient= function (...)
  {
    .modif.data=function (...)
    {

      assign("id.modif", as.numeric(tkcurselection(tlm)) + 1, envir = as.environment(pos))
      tkdestroy(tt.modif)
      m.data=Res$PT.data[id.modif,]
      win.m <- tktoplevel()
      dlt.      <- tclVar(m.data[,2])
      level.    <- tclVar(m.data[,1])
      followup. <- tclVar(m.data[,4])
      lab.id. <- tklabel(win.m,text=paste("Patient N_",id.modif),font=PoliceGenerale)
      tkgrid(lab.id., row=0, columnspan=2)
      lab.level. <- tklabel(win.m,text=paste("Dose level"),font=PoliceGenerale)
      tkgrid(lab.level., row=1, columnspan=2)
      assign("win.level", tk2entry(win.m, width = "3", textvariable = level.),envir = as.environment(pos))
      tkgrid(win.level,row=1,column=2)
      lab.dlt. <- tklabel(win.m,text=paste("DLT (1= 'Yes', 0='No')"),font=PoliceGenerale)
      tkgrid(lab.dlt., row=3, columnspan=2)
      assign("win.dlt", tk2entry(win.m, width = "3", textvariable = dlt.),envir = as.environment(pos))
      tkgrid(win.dlt,row=3,column=2)
      lab.fup. <- tklabel(win.m,text=paste("follow-up","<=",Study$Obs.wind),font=PoliceGenerale)
      tkgrid(lab.fup., row=5, columnspan=2)
      assign("win.fup", tk2entry(win.m, width = "3", textvariable = followup.),envir = as.environment(pos))
      tkgrid(win.fup,row=5,column=2)


      onOK <- function() {
        new.dlt.  <- as.numeric(tclvalue(tkget(win.dlt)))
        new.level <- as.numeric(tclvalue(tkget(win.level)))
        new.fup   <- as.numeric(tclvalue(tkget(win.fup)))
        Res$PT.data[id.modif,c(2,1,4)] <- c(new.dlt.,new.level,new.fup)
        e$TEWOC. <- .titeewoc(prior=Study$Prior, target=Study$Target, tox=Res$PT.data[,2],
                              level=Res$PT.data[,1], followup=Res$PT.data[,4], obswin=Study$Obs.wind,
                              pointest=Study$Pointest, method="rjags", model=Study$Model, intcpt=Study$Intcpt,
                              p.dist=Study$P.dist, p1=Study$P1, p2=Study$P2, alphap=Study$Alphap, constrain= Study$Constrain)

        Res$MTD <- e$TEWOC.$dose.rec
        TEWOC. <- e$TEWOC.
        save(Res,Study,TEWOC.,file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'))
        load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'))
        tkdestroy(win.m)
        assign("Res", Res,envir = as.environment(pos))
        assign("Study", Study,envir = as.environment(pos))
        .ReCap()
        .ReCap.Resultat()
      }
      win.OK <-tkbutton(win.m, text = "OK", width = 10, command = onOK, bg="cornflower blue")
      tkgrid(win.OK)
    }

    assign("m.data", Res$PT.data,envir = as.environment(pos))
    if (is.null(m.data))
    {
      msg <- paste("No patient included",sep="")
      tkmessageBox(message=msg)
    }
    if (!is.null(m.data))
    {
      if (nrow(m.data)>=1)
      {
        assign("tt.modif", tktoplevel(),envir = as.environment(pos))
        tkwm.geometry(tt.modif, "+650+400")
        assign("tlm", tklistbox(tt.modif,height=nrow(m.data)+1,selectmode="single",background="white"),envir = as.environment(pos))
        tkgrid(tklabel(tt.modif,text="Select a patient",font=PoliceGenerale))
        tkgrid(tlm)
        patlist=paste(" Patient N_", as.character(m.data$patid),sep="")
        for (i in (1:length(patlist)))
        {
          tkinsert(tlm,"end",patlist[i])
        }
        tkselection.set(tlm,0)
        OK.but <-tkbutton(tt.modif,text="  OK  ",command=.modif.data,bg="cornflower blue")
        tkgrid(OK.but)
      }
    }
  }


  disabled.Recap=function(...)
  {

    load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    priorp=paste(Study$Prior[1])
    for( i in 2 : length(Study$Prior))
    {
      priorp=paste(priorp,paste(Study$Prior[i]),sep=" , ")
    }

    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    assign("Rec.mm0", tkframe(include, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.mm0, row=0, column=0, rowspan=16)

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=0, column=0)

    NS<-tklabel(Rec.mm0,text=Study$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=2, column=0)

    Titre1<-tklabel(Rec.mm0,text="Characteristics of the study")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)

    affiche_4.0<-tklabel(Rec.mm0,text=paste("Number of dose levels "))
    affiche_4.1<-tklabel(Rec.mm0,text=paste(Study$Nb_Dl))

    affiche_2.0<-tklabel(Rec.mm0,text=paste("Initial estimated probabilities " ))
    affiche_2.1<-tklabel(Rec.mm0,text=paste(priorp))

    affiche_3.0<-tklabel(Rec.mm0,text=paste("Target DLT rate"))
    affiche_3.1<-tklabel(Rec.mm0,text=paste(Study$Target))

    affiche_1.0<-tklabel(Rec.mm0,text=paste("Method "))
    affiche_1.1<-tklabel(Rec.mm0,text=paste("Time-to-Event Escalation With Overdose Control (Tite-EWOC)"))

    affiche_5.0<-tklabel(Rec.mm0,text=paste("Statistical model "))
    affiche_5.1<-tklabel(Rec.mm0,text=paste(ifelse(Study$Model=="ht","Hyperbolic tangent",ifelse(Study$Model=="logit1","Logistic","Power"))))

    affiche_6.0<-tklabel(Rec.mm0,text=paste("Stopping rules "))
    affiche_6.1<-tklabel(Rec.mm0,text=paste("Sample size"))

    affiche_7.0<-tklabel(Rec.mm0,text=paste("Maximal number of patients "))
    affiche_7.1<-tklabel(Rec.mm0,text=paste(Study$Npat))

    affiche_10.0<-tklabel(Rec.mm0,text=paste("Observation window "))
    affiche_10.1<-tklabel(Rec.mm0,text=paste(Study$Obs.wind))

    tkgrid(affiche_1.0, row=4, column=1,  sticky="w")
    tkgrid(affiche_1.1, row=4, column=2,  sticky="w")
    tkconfigure(affiche_1.0, font=Policeligne)
    tkconfigure(affiche_1.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_2.0, row=5, column=1, sticky="w")
    tkgrid(affiche_2.1, row=5, column=2, sticky="w")
    tkconfigure(affiche_2.0, font=Policeligne)
    tkconfigure(affiche_2.1, font=Policeligne , foreground="blue")


    tkgrid(affiche_3.0, row=6, column=1, sticky="w")
    tkgrid(affiche_3.1, row=6, column=2,  sticky="w")
    tkconfigure(affiche_3.0, font=Policeligne )
    tkconfigure(affiche_3.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_4.0, row=7, column=1, sticky="w")
    tkgrid(affiche_4.1, row=7, column=2,  sticky="w")
    tkconfigure(affiche_4.0, font=Policeligne )
    tkconfigure(affiche_4.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_5.0, row=8, column=1,  sticky="w")
    tkgrid(affiche_5.1, row=8, column=2,sticky="w")
    tkconfigure(affiche_5.0, font=Policeligne )
    tkconfigure(affiche_5.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_6.0, row=9, column=1,  sticky="w")
    tkgrid(affiche_6.1, row=9, column=2,  sticky="w")
    tkconfigure(affiche_6.0, font=Policeligne)
    tkconfigure(affiche_6.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_7.0, row=10, column=1,  sticky="w")
    tkgrid(affiche_7.1, row=10, column=2,  sticky="w")
    tkconfigure(affiche_7.0, font=Policeligne )
    tkconfigure(affiche_7.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_10.0, row=11, column=1,  sticky="w")
    tkgrid(affiche_10.1, row=11, column=2,  sticky="w")
    tkconfigure(affiche_10.0, font=Policeligne )
    tkconfigure(affiche_10.1, font=Policeligne , foreground="blue")

    Titre2<-tklabel(Rec.mm0,text="inclusion")
    tkgrid(Titre2, row=12, column=1,sticky="w")
    tkconfigure(Titre2, font=PoliceTitre)

    affiche_8.0<-tklabel(Rec.mm0,text=paste("Number of patients included:"))
    affiche_8.1<-tklabel(Rec.mm0,text=paste(Res$Pat.included))

    affiche_9.0<-tklabel(Rec.mm0,text=paste("Estimated MTD:"))
    affiche_9.1<-tklabel(Rec.mm0,text=paste(Res$MTD))

    tkgrid(affiche_8.0, row=13, column=1,  sticky="w")
    tkgrid(affiche_8.1, row=13, column=2,  sticky="w")
    tkconfigure(affiche_8.0, font=Policeligne )
    tkconfigure(affiche_8.1, font=Policeligne , foreground="blue")

    tkgrid(affiche_9.0, row=14, column=1,sticky="w")
    tkgrid(affiche_9.1, row=14, column=2,sticky="w")
    tkconfigure(affiche_9.0, font=Policeligne)
    tkconfigure(affiche_9.1, font=Policeligne , foreground="blue")

    l<-tklabel(Rec.mm0,text="")
    tkgrid(l, row=15, column=0)

    assign("Rec.Win_TEWOC", tkframe(include,relief="groove",borderwidth=3),envir = as.environment(pos))
    Save=tkbutton(Rec.Win_TEWOC,text="New patient",width=30,state="disabled",bg="cornflower blue")
    tkgrid(Rec.Win_TEWOC, row=16, column=0)
    tkgrid(Save, row=0, column=0)
    Modif=tkbutton(Rec.Win_TEWOC,text="Modif patient data",width=30,state="disabled",bg="cornflower blue")
    tkgrid(Modif, row=0, column=1)

  }

  .ReCap.Resultat <- function ()
  {

    load(file =paste(paste(Names_Study,'tewoc',sep='-'),'RData',sep='.'),envir = as.environment(pos))

    ResultsPat <- function() {

      myRarray. <- data.frame(dose_level=Res$PT.data[,1],
                              Toxicity=Res$PT.data[,2],Follow_up=Res$PT.data[,4])
      rownames(myRarray.) <- c(1:dim(myRarray.)[1])
      rownames(myRarray.) <- paste0("Pat N_ ",rownames(myRarray.))
      tclarray.  <- tclArrayVar(myRarray.)
      print.tclArrayVar(tclarray.)

    }
    ResultsbyLevel<- function() {

      prior=e$TEWOC.$prior
      n.patient=e$TEWOC.$nb.pat.level
      total.tox=e$TEWOC.$nb.tox.level
      m.weight=round(e$TEWOC.$weights.m.level,3)
      quantiles=signif(t(e$TEWOC.$quantiles.ptox[c(1,3,5),]),3)
      assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=prior, Nb.patient=n.patient,
                                     Total.tox=total.tox,mean.weight=m.weight,q=quantiles),envir = as.environment(pos))
      rownames(myRarray2) <- c(1:dim(myRarray2)[1])
      rownames(myRarray2) <- paste0("dose ",rownames(myRarray2))
      tclarray2 <- tclArrayVar(myRarray2)
      print.tclArrayVar(tclarray2)
    }

    ExportRes <- function() {

      filef <- tclvalue(tkgetSaveFile())
      if (filef != "") {
        filesave=paste(filef,"xlsx",sep=".")


        pdf(paste(filef,"pdf",sep="."))

        n=length(Res$PT.data[,1])
        x=c(1:n)
        y=Res$PT.data[,1]
        z=Res$PT.data[,2]
        Data=data.frame(x=x,y=y,z=z)
        plot(x,y,pch=16,cex=2,col=(z+1),type='b',axes=FALSE,xlab="Patient number",ylab='Dose Level',main="Patients, Dose level and toxicity")
        axis(1, lwd = 2)
        axis(side=2,1:8, lwd = 2)
        legend("bottomright",inset = c(0.0, -0.19),  lwd=c(2.5,2.5),legend=c("Non Toxicity","Toxicity"), col = c(1,2), pch = 16,cex=0.8,xpd = NA)
        dev.off()

        ggsave(plot=.plot.titeewoc(e$TEWOC.) , filename=paste(paste(filef,"boxplot",sep=""),"pdf",sep="."))
        myRarray. <- data.frame(dose_level=Res$PT.data[,1],
                                Toxicity=Res$PT.data[,2],Follow_up=Res$PT.data[,4])
        rownames(myRarray.) <- c(1:dim(myRarray.)[1])
        rownames(myRarray.) <- paste0("Pat N_ ",rownames(myRarray.))
        prior=e$TEWOC.$prior
        n.patient=e$TEWOC.$nb.pat.level
        total.tox=e$TEWOC.$nb.tox.level
        m.weight=round(e$TEWOC.$weights.m.level,3)
        quantiles=signif(t(e$TEWOC.$quantiles.ptox[c(1,3,5),]),3)
        assign("myRarray2", data.frame(Level=c(1:Study$Nb_Dl),Prior=prior, Nb.patient=n.patient,
                                       Total.tox=total.tox,mean.weight=m.weight,q=quantiles), envir = as.environment(pos))
        rownames(myRarray2) <- c(1:dim(myRarray2)[1])
        rownames(myRarray2) <- paste0("dose ",rownames(myRarray2))
        wb <- createWorkbook()
        sheet1 <- addWorksheet(wb, sheetName="By patients")
        sheet2 <- addWorksheet(wb, sheetName="By dose level")
        style <- createStyle(fontSize = 12, fontColour = "black",
                             textDecoration = c("bold", "italic", "underline"),
                             halign = "center", valign = "center", border = "Bottom",
                             fgFill = "gray")

        setColWidths(wb, sheet1, cols=1:3, widths = 14.43)
        setColWidths(wb, sheet2, cols=1:8, widths = 14.43)

        writeDataTable(wb,sheet1,x=myRarray.)
        writeDataTable(wb,sheet2,x=myRarray2)

        addStyle(wb,sheet1,style,cols=1:3,rows=1)
        addStyle(wb,sheet2,style,cols=1:8,rows=1)

        saveWorkbook(wb, filesave, overwrite = T)
      }
    }

    GrapheResults = function(...)
    {
      n=length(Res$PT.data[,1])
      x=c(1:n)
      y=Res$PT.data[,1]
      z=Res$PT.data[,2]
      Data=data.frame(x=x,y=y,z=z)
      plot(x,y,pch=16,cex=2,col=(z+1),type='b',axes=FALSE,xlab="Patient number",ylab='Dose Level',main="Patients, Dose level and toxicity")
      axis(1, lwd = 2)
      axis(side=2,1:8, lwd = 2)
      legend("bottomright",inset = c(0.0, -0.19),  lwd=c(2.5,2.5),legend=c("Non Toxicity","Toxicity"), col = c(1,2), pch = 16,cex=0.8,xpd = NA)
    }


    GrapheGG = function()
    {
      print(.plot.titeewoc(e$TEWOC.))
    }

    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=9)

    l1<-tklabel(Resultas,text="")
    l2<-tklabel(Resultas,text="")
    l3<-tklabel(Resultas,text="")
    l4<-tklabel(Resultas,text="")
    l5<-tklabel(Resultas,text="")

    Titre.1=tklabel(Resultas,text="Patient Summary",font=PoliceTitre)
    Titre.2=tklabel(Resultas,text="Dose level Summary",font=PoliceTitre)
    Titre.3=tklabel(Resultas,text="Toxicity plot",font=PoliceTitre)
    Titre.4=tklabel(Resultas,text="Toxicity Prob boxplot",font=PoliceTitre)
    Titre.5=tklabel(Resultas,text="Export results",font=PoliceTitre)

    button1=tkbutton(Resultas ,text="Summary",width=20,command=ResultsPat,bg="cornflower blue")
    button2=tkbutton(Resultas ,text="Summary",width=20,command=ResultsbyLevel,bg="cornflower blue")
    button3=tkbutton(Resultas ,text="Graphic",width=20,command=GrapheResults,bg="cornflower blue")
    button4=tkbutton(Resultas ,text="Graphic",width=20,command=GrapheGG,bg="cornflower blue")
    button5=tkbutton(Resultas ,text="Export",width=20,command=ExportRes,bg="cornflower blue")

    tkgrid(l1, row=0, column=0)
    tkgrid(Titre.1, row=1, column=1, sticky="w")
    tkconfigure(Titre.1, font=PoliceTitre)
    tkgrid(button1, row=1, column=2)

    tkgrid(l2, row=2, column=1)
    tkgrid(Titre.2, row=3, column=1, sticky="w")
    tkconfigure(Titre.2, font=PoliceTitre)
    tkgrid(button2, row=3, column=2)

    tkgrid(l3, row=4, column=1)
    tkgrid(Titre.3, row=5, column=1, sticky="w")
    tkconfigure(Titre.3, font=PoliceTitre)
    tkgrid(button3, row=5, column=2)

    tkgrid(l4, row=6, column=1)
    tkgrid(Titre.4, row=7, column=1, sticky="w")
    tkconfigure(Titre.4, font=PoliceTitre)
    tkgrid(button4, row=7, column=2)

    tkgrid(l5, row=8, column=1)
    tkgrid(Titre.5, row=9, column=1, sticky="w")
    tkgrid(button5, row=9, column=2)
    tkconfigure(button5,padx=2)
    tkconfigure(Titre.5, font=PoliceTitre)
  }

  disabled.TEWOC=function()
  {
    tkdestroy(Win_TEWOC)
    frameInitTEWOC()
    Nb_Niv_Doses <- Study$Nb_Dl
    assign("m2", tkframe(Required, relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    assign("Saisi", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=15)

    assign("Saisi.save", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (Study$Prior[i])
      tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=4)
      tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=5)
    }

    SliderValue<-tclVar(Nb_Niv_Doses)
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=2, sticky="w")

    Titre <-tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar (Study$Target)
    Target <-tkentry(Saisi, width=5, textvariable=n.iter, state="disabled")
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Tmod = tklabel(Saisi,text="Model",font=PoliceGenerale)
    n.iter <- switch (Study$Model, ht="Hyperbolique tangent",
                      power="Power", logit1="1-Logistic")
    Model <- tk2spinbox(Saisi, values = c( n.iter,""),width=19, state="disabled")

    tkgrid(Tmod , row=6,column=0, sticky="w")
    tkgrid(Model, row=6,column=1, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale)
    intcpt <- if (is.null(Study$Intcpt))
    { c("NA")  }
    else { Study$Intcpt}
    n.intcpt <- tclVar(intcpt)
    assign("intcpt.", tkentry(Saisi, width=10, textvariable=n.intcpt, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt., row=6,column=2, sticky="w")
    tkgrid(intcpt., row=6,column=3, sticky="w")

    Tstop=tklabel(Saisi,text="At the moment an unique stopping rule is proposed: sample size                          ",
                  font=PoliceGenerale)
    tkgrid(Tstop, row=7,column=0,columnspan=4, sticky="w")

    Tnum=tklabel(Saisi,text="Number of patients corresponding",font=PoliceGenerale)
    n.iter <- tclVar(Study$Npat)
    N <- tkentry(Saisi, width=5, textvariable=n.iter, state="disabled")
    tkgrid(Tnum, row=8, column=0, columnspan=2, sticky="w")
    tkgrid(N, row=8, column=2, sticky="w")

    Tpdist = tklabel(Saisi,text="Prior alpha dist. ",font=PoliceGenerale)
    n.iter <- switch (Study$P.dist, unif="Uniform [p1,p2]",
                      gamma="Gamma (p1,p2)")
    Pdist <- tk2spinbox(Saisi, values = c(n.iter,""), width=15, state="disabled")
    tkgrid(Tpdist , row=9,column=0, columnspan=2, sticky="w")
    tkgrid(Pdist, row=9,column=1, sticky="w")

    Talpha=tklabel(Saisi,text="Prior alpha value (DFLT=1)", font=PoliceGenerale)
    Tp1=tklabel(Saisi,text="p1",font=PoliceGenerale)
    Tp2=tklabel(Saisi,text="p2",font=PoliceGenerale)
    n.al <- tclVar(Study$Alphap); n.p1 <- tclVar(Study$P1)
    n.p2 <- tclVar(Study$P2)
    alpha<-tkentry(Saisi, width=7, textvariable=n.al, state="disabled")
    p1<-tkentry(Saisi, width=7, textvariable=n.p1, state="disabled")
    p2<-tkentry(Saisi, width=7, textvariable=n.p2, state="disabled")

    tkgrid(Tp1, row=10, column=0, sticky="w")
    tkgrid(p1, row=10, column=1, sticky="w")
    tkgrid(Tp2, row=10, column=2, sticky="w")
    tkgrid(p2, row=10, column=3, sticky="w")
    tkgrid(Talpha, row=11, column=0, columnspan=3,sticky="w")
    tkgrid(alpha, row=11, column=2, sticky="w")

    Tpoint = tklabel(Saisi, text="Pointest in [0, 0.5]",font=PoliceGenerale)
    n. <- tclVar(Study$Pointest)
    point <- tkentry(Saisi, width=10, textvariable=n., state="disabled")
    tkgrid(Tpoint, row=12, column=0, columnspan=2, sticky="w")
    tkgrid(point, row=12, column=1, sticky="w")

    Titrec =tklabel(Saisi,text="Dose skipping constraint",font=PoliceGenerale)
    const1 <- tkradiobutton(Saisi, state="disabled") ;
    const2 <- tkradiobutton(Saisi, state="disabled")
    rbValue.const <- tclVar(as.numeric(Study$Constrain))
    tkconfigure(const1,variable=rbValue.const,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2,variable=rbValue.const,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec, row=12, column=2, columnspan=2, sticky="w")
    tkgrid(const1, row=12,  column=4, sticky="w")
    tkgrid(const2, row=13,column=4, sticky="w")

    Twind<-tklabel(Saisi, text="Observation window",font=PoliceGenerale)
    n.obs <- tclVar(Study$Obs.wind)
    Wind<-tkentry(Saisi, width=10, textvariable=n.obs, state="disabled")
    tkgrid(Twind, row=13,column=0,columnspan=2, sticky="w")
    tkgrid(Wind, row=13,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(Study$Name)
    names.S<-tkentry(Saisi, width=18, textvariable=Nam_, state="disabled")
    tkgrid(labage, row=14, column=0, sticky="w")
    tkgrid(names.S, row=14, column=1,columnspan=2, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,state="disabled", bg="cornflower blue")
    tkgrid(Save.rec, row=1)

  }

  NewTEWOC=function()
  {
    tkdestroy(Win_TEWOC)
    frameInitTEWOC()

    .Crea.f=function(...)
    {
      Nb_Niv_Doses <- as.numeric(tclvalue((SliderValue)))

      for ( i in 1: Nb_Niv_Doses)
      {
        r=17 + i
        tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=0)
        tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8),envir = as.environment(pos)),row=r,column=1)
      }
      if( Nb_Niv_Doses!=8)
      {
        for ( i in (Nb_Niv_Doses+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('lab',i,sep=''),tklabel(m2, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=0)
          tkgrid(assign(paste('val',i,sep=''),tkentry(m2, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=1)
        }
      }
    }
    disp.int=function()
    {
      if (!(tclvalue(tkget(Model))=="1-Logistic"))
      {
        Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt.", tkentry(Saisi, width=10, state="disabled"),envir = as.environment(pos))
        tkgrid(Tintcpt., row=6,column=2, sticky="w")
        tkgrid(intcpt., row=6,column=3, sticky="w")
      }
      else {
        Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale)
        assign("intcpt.", tkentry(Saisi, width=10),envir = as.environment(pos))
        tkgrid(Tintcpt., row=6,column=2, sticky="w")
        tkgrid(intcpt., row=6,column=3, sticky="w")
      }
    }

    .Sauvgarde.Study=function(...)
    {
      Nb_Dl=as.numeric(tclvalue(SliderValue))
      prior=NULL
      if (Nb_Dl==2) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))))}
      if (Nb_Dl==3) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))))}
      if (Nb_Dl==4) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))))}
      if (Nb_Dl==5) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))))}
      if (Nb_Dl==6) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))))}
      if (Nb_Dl==7) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),
                             as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),as.numeric(tclvalue(tkget(val7))))}
      if (Nb_Dl==8) {prior=c(as.numeric(tclvalue(tkget(val1))),as.numeric(tclvalue(tkget(val2))),as.numeric(tclvalue(tkget(val3))),
                             as.numeric(tclvalue(tkget(val4))),as.numeric(tclvalue(tkget(val5))),as.numeric(tclvalue(tkget(val6))),
                             as.numeric(tclvalue(tkget(val7))),as.numeric(tclvalue(tkget(val8))))}
      npat=NULL
      alphap=1
      target=as.numeric(tclvalue(tkget((Target))))
      pointest=as.numeric(tclvalue(tkget(point)))
      const=as.logical(as.numeric(tclvalue((rbValue.const))))

      model.=tclvalue(tkget(Model))
      intcpt=NULL
      if (model. == "Hyperbolic tangent") model = "ht"
      if (model. == "1-Logistic") {model = "logit1"; intcpt=as.numeric(tclvalue(tkget(intcpt.)))}
      if (model. == "Power") model = "power"

      pdist.=tclvalue(tkget(Pdist))
      if (pdist. == "Uniform [p1,p2]") pdist = "unif"
      if (pdist. == "Gamma (p1,p2)") pdist = "gamma"

      npat=as.numeric(tclvalue(tkget((N))))
      alphap=as.numeric(tclvalue(tkget((alpha))))
      p2=as.numeric(tclvalue(tkget((p2))))
      p1=as.numeric(tclvalue(tkget((p1))))
      twind=as.numeric(tclvalue(tkget(Wind)))

      assign("Names_Study", tclvalue(tkget(names.S)),envir = as.environment(pos))

      if (npat <=0) stop('Number of patients to be enrolled in the study <=0')
      if (npat > 100) cat("\n Warning: Number of patients to be enrolled in the study > 100")
      if (twind <= 0 ) stop('Observation window incorrect')

      if (target>0.55) cat("\n Warning: Target DLT rate too high")
      if (target<0.15) cat("\n Warning: Target DLT rate too low")
      if ((pointest < 0) | (pointest > 0.5)) stop('Pointest parameter must be within [0,0.5]')
      if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
      if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')

      assign("Study", list(Name=paste(Names_Study), Nb_Dl=Nb_Dl, Prior=prior, Target=target,
                           P.dist=pdist, P1=p1, P2=p2, Model=model, Alphap=alphap, Intcpt=intcpt,
                           Method="rjags", Npat=npat, Pointest=pointest, Obs.wind=twind, Constrain=const),
             envir = as.environment(pos))

      MTD <- max(which.min(abs(Study$Prior-Study$Target)),1)
      Pat_Include  <- 0
      PT.data <- NULL
      assign ("Res", list(MTD=MTD,Pat.included=Pat_Include,PT.data=PT.data,Obs.wind=Study$Obs.wind),
              envir = as.environment(pos))
      save(Res,Study,file =paste(paste(Study$Name,'tewoc',sep='-'),'RData',sep='.'))
      .ReCap()
      Study.file=function()
      {
        a00=paste(Study$Name)
        a0=paste('Name of study',paste(Study$Name),sep=':')
        a1=paste('Method','Tite-EWOC',sep=':')
        a2='Time-to-Event Escalation With Overdose Control (Tite-EWOC) for Phase I Clinical Trials'
        datee <- paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(Study$Name),'tewoc', sep='.'))
      }
      Study.file()
      tk2notetab.select(net.TEWOC , "Include")

    }

    assign("Saisi", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)
    tkgrid(Saisi, row=1, column=0, columnspan=4, rowspan=15)

    assign("Saisi.save", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi.save, FALSE)
    tkgrid(Saisi.save , row=27, column=0, sticky="se")

    assign("m2", tkframe(Required, relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=8)

    SliderValue<-tclVar('8')
    slider_Nb_DL <- tkscale(Saisi, from=2, to=8,showvalue=T, variable=SliderValue, resolution=1, orient="horiz",command=.Crea.f)
    tkgrid(tklabel(Saisi,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL , row=1,column=2, sticky="w")

    Titre <-tklabel(Saisi,text="Target DLT rate",font=PoliceGenerale)
    Target <-tkentry(Saisi, width=5)
    tkgrid(Titre, row=2, column=0, sticky="w")
    tkgrid(Target, row=2, column=1, sticky="w")

    Tmod = tklabel(Saisi,text="Model",font=PoliceGenerale)
    Model <- tk2spinbox(Saisi, values = c("Hyperbolic tangent","1-Logistic","Power"),width=17, command=disp.int)

    tkgrid(Tmod , row=6,column=0, sticky="w")
    tkgrid(Model, row=6,column=1, sticky="w")

    Tintcpt.<-tklabel(Saisi, text="intercept",font=PoliceGenerale, state="disabled")
    assign("intcpt.", tkentry(Saisi, width=10, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt., row=6,column=2, sticky="w")
    tkgrid(intcpt., row=6,column=3, sticky="w")

    Tstop=tklabel(Saisi,text="At the moment an unique stopping rule is proposed: sample size                          ",
                  font=PoliceGenerale)
    tkgrid(Tstop, row=7,column=0,columnspan=4, sticky="w")

    Tnum=tklabel(Saisi,text="Number of patients corresponding",font=PoliceGenerale)
    N <- tkentry(Saisi, width=5)
    tkgrid(Tnum, row=8, column=0, columnspan=2, sticky="w")
    tkgrid(N, row=8, column=2, sticky="w")

    Tpdist = tklabel(Saisi,text="Prior alpha dist. ",font=PoliceGenerale)
    Pdist <- tk2spinbox(Saisi, values = c("Uniform [p1,p2]","Gamma (p1,p2)"),width=15)
    tkgrid(Tpdist , row=9,column=0, columnspan=2, sticky="w")
    tkgrid(Pdist, row=9,column=1, sticky="w")

    Talpha=tklabel(Saisi,text="Prior alpha value (DFLT=1)", font=PoliceGenerale)
    Tp1=tklabel(Saisi,text="p1",font=PoliceGenerale)
    Tp2=tklabel(Saisi,text="p2",font=PoliceGenerale)
    alpha<-tkentry(Saisi, width=7)
    p1<-tkentry(Saisi, width=7);p2<-tkentry(Saisi, width=7)

    tkgrid(Tp1, row=10, column=0, sticky="w")
    tkgrid(p1, row=10, column=1, sticky="w")
    tkgrid(Tp2, row=10, column=2, sticky="w")
    tkgrid(p2, row=10, column=3, sticky="w")
    tkgrid(Talpha, row=11, column=0, columnspan=3,sticky="w")
    tkgrid(alpha, row=11, column=2, sticky="w")

    Tpoint = tklabel(Saisi, text="Pointest in [0, 0.5]",font=PoliceGenerale)
    point <- tkentry(Saisi, width=10)
    tkgrid(Tpoint, row=12, column=0, columnspan=2, sticky="w")
    tkgrid(point, row=12, column=1, sticky="w")

    Titrec=tklabel(Saisi,text="Dose skipping constraint",font=PoliceGenerale)
    const1 <- tkradiobutton(Saisi) ;
    const2 <- tkradiobutton(Saisi)
    rbValue.const <- tclVar(1)
    tkconfigure(const1,variable=rbValue.const,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2,variable=rbValue.const,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec, row=12, column=2, columnspan=2, sticky="w")
    tkgrid(const1, row=12,  column=4, sticky="w")
    tkgrid(const2, row=13,column=4, sticky="w")

    Twind<-tklabel(Saisi, text="Observation window",font=PoliceGenerale)
    Wind<-tkentry(Saisi, width=10)
    tkgrid(Twind, row=13,column=0,columnspan=2, sticky="w")
    tkgrid(Wind, row=13,column=1, sticky="w")

    labage<-tklabel(Saisi, text="Name of study ",font=PoliceGenerale)
    names.S<-tkentry(Saisi, width=18)

    tkgrid(labage, row=14, column=0, sticky="w")
    tkgrid(names.S, row=14, column=1,columnspan=2, sticky="w")

    Save.rec=tkbutton(Saisi.save ,text="Save study",width=25,command=.Sauvgarde.Study, bg="cornflower blue")
    tkgrid(Save.rec, row=1)
  }

  .RecapSim=function()
  {

    .Save_sim=function()
    {
      Study.file=function()
      {
        a00=paste(StudyS$Name)
        a0=paste('Name of study',paste(StudyS$Name),sep=':')
        a1=paste('Method','Tite-EWOC',sep=':')
        a2='Time-to-Event Escalation With Overdose Control (Tite-EWOC) for Phase I Clinical Trials'
        datee<-paste(format(Sys.time(), "%A %d %B %Y"))
        a3=paste('Date of creation',datee)
        Data=data.frame(c(a00,a0,a1,a2,a3))
        write.table(Data,paste(paste(StudyS$Name),'tewsim', sep='.'))
      }
      Study.file()
      ResSim <- e$ResSim
      save(ResSim,StudyS,file =paste(paste(StudyS$Name,'tewsim',sep='-'),'RData',sep='.'))
      Save.infos <- tkmessageBox(title = "Data storage Infos",
                                 message = "File saved in the current working directory of the R proces", icon = "info", type = "ok")
    }
    PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
    Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)


    assign("Rec.fr", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.fr, row=0, column=0, rowspan=20)


    PriorRes=paste(round(StudyS$Prior[1],3))
    TrueRes=paste(round(StudyS$True[1],3))
    MTDRes=paste(round(e$ResSim$pct.dose.rec[1],3))
    ToxRes=paste(round(e$ResSim$nb.tox.level[1]/e$ResSim$nsim,3))
    LevelRes=paste(round(e$ResSim$pat.level[1],3))

    for( i in 2 : length(StudyS$Prior))
    {
      PriorRes=paste(PriorRes,paste(round(StudyS$Prior[i],3)),sep=", ")
      TrueRes=paste(TrueRes,paste(round(StudyS$True[i],3)),sep=", ")
      MTDRes=paste(MTDRes,paste(round(e$ResSim$pct.dose.rec[i],3)),sep=", ")
      LevelRes=paste(LevelRes,paste(round(e$ResSim$pat.level[i],3)),sep=", ")
      ToxRes=paste(ToxRes,paste(round(e$ResSim$nb.tox.level[i]/e$ResSim$nsim,3)),sep=", ")
    }

    NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
    tkgrid(NS, row=1, column=1,  sticky="w")
    tkconfigure(NS, font=PoliceEtude, foreground="white")

    Titre1<-tklabel(Rec.fr,text="Results of the simulation")
    tkgrid(Titre1, row=3, column=1)
    tkconfigure(Titre1, font=PoliceTitre)
    model <- switch (StudyS$Model, ht="hyperbolique tangent",
                     power="Power", logit1="1-logistic")
    aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
    aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
    aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
    aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
    aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
    aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
    aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
    aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
    aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
    aff_1.1<-tklabel(Rec.fr,text=paste("Time-to-Event Escalation With Overdose Control "))
    aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
    aff_6.1<-tklabel(Rec.fr,text=paste(model))
    aff_8.0<-tklabel(Rec.fr,text=paste("% recommendation MTD"))
    aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
    aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
    aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$samplesize))
    aff_9.0<-tklabel(Rec.fr,text=paste("Percentage of patients treated by dose level"))
    aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
    aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
    aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
    aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
    aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))
    aff_12.0<-tklabel(Rec.fr,text=paste("Mean inter-patient arrival time"))
    aff_12.1<-tklabel(Rec.fr,text=paste(StudyS$Tmean.arrival))
    aff_13.0<-tklabel(Rec.fr,text=paste("Observation window"))
    aff_13.1<-tklabel(Rec.fr,text=paste(StudyS$Obs.wind))

    tkgrid(aff_1.0, row=4, column=1,  sticky="w")
    tkgrid(aff_1.1, row=4, column=2,  sticky="w")
    tkconfigure(aff_1.0, font=Policeligne)
    tkconfigure(aff_1.1, font=Policeligne , foreground="blue")

    tkgrid(aff_2.0, row=6, column=1, sticky="w")
    tkgrid(aff_2.1, row=6, column=2, sticky="w")
    tkconfigure(aff_2.0, font=Policeligne)
    tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

    tkgrid(aff_3.0, row=7, column=1, sticky="w")
    tkgrid(aff_3.1, row=7, column=2,  sticky="w")
    tkconfigure(aff_3.0, font=Policeligne )
    tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

    tkgrid(aff_4.0, row=8, column=1, sticky="w")
    tkgrid(aff_4.1, row=8, column=2,  sticky="w")
    tkconfigure(aff_4.0, font=Policeligne )
    tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

    tkgrid(aff_5.0, row=9, column=1,  sticky="w")
    tkgrid(aff_5.1, row=9, column=2,sticky="w")
    tkconfigure(aff_5.0, font=Policeligne )
    tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

    tkgrid(aff_6.0, row=10, column=1,  sticky="w")
    tkgrid(aff_6.1, row=10, column=2,  sticky="w")
    tkconfigure(aff_6.0, font=Policeligne)
    tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

    tkgrid(aff_7.0, row=11, column=1,  sticky="w")
    tkgrid(aff_7.1, row=11, column=2,  sticky="w")
    tkconfigure(aff_7.0, font=Policeligne )
    tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

    tkgrid(aff_11.0, row=12, column=1,  sticky="w")
    tkgrid(aff_11.1, row=12, column=2,  sticky="w")
    tkconfigure(aff_11.0, font=Policeligne )
    tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

    tkgrid(aff_12.0, row=13, column=1,  sticky="w")
    tkgrid(aff_12.1, row=13, column=2,  sticky="w")
    tkconfigure(aff_12.0, font=Policeligne )
    tkconfigure(aff_12.1, font=Policeligne , foreground="blue")

    tkgrid(aff_13.0, row=14, column=1,  sticky="w")
    tkgrid(aff_13.1, row=14, column=2,  sticky="w")
    tkconfigure(aff_13.0, font=Policeligne )
    tkconfigure(aff_13.1, font=Policeligne , foreground="blue")

    tkgrid(aff_8.0, row=15, column=1,  sticky="w")
    tkgrid(aff_8.1, row=15, column=2,  sticky="w")
    tkconfigure(aff_8.0, font=Policeligne )
    tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

    tkgrid(aff_9.0, row=16, column=1,  sticky="w")
    tkgrid(aff_9.1, row=16, column=2,  sticky="w")
    tkconfigure(aff_9.0, font=Policeligne )
    tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

    tkgrid(aff_10.0, row=17, column=1,  sticky="w")
    tkgrid(aff_10.1, row=17, column=2,  sticky="w")
    tkconfigure(aff_10.0, font=Policeligne )
    tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

    assign("Rec.save_", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
    tkgrid(Rec.save_, row=20, column=0)

    l<-tklabel(Rec.save_,text="")
    tkgrid(l, row=2)
    ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,round(e$ResSim$pct.dose.rec,3),
                              round(e$ResSim$pat.level,3),round(e$ResSim$nb.tox.level,3)))
    colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.(%)",
                         "(%)patient","Av. tox. ")
    WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                      command=function()ExcellExport(dframe=ResExp_),bg="cornflower blue")
    tkgrid(WordExp_, row=3)
    blank<-tklabel(Rec.save_,text="      ")
    tkgrid(blank, row=4)
    SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,command=.Save_sim,bg="cornflower blue")
    tkgrid(SaveSim_, row=5)
  }


  disabled.SimTEWOC=function()
  {

    .RecapSimB=function()
    {

      PoliceEtude <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=15)
      PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=12)
      Policeligne <- tkfont.create(family="calibri",underline=FALSE, size=11)

      assign("Rec.fr", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=20)


      assign("Rec.fr", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
      tkgrid(Rec.fr, row=0, column=0, rowspan=20)


      PriorRes=paste(round(StudyS$Prior[1],3))
      TrueRes=paste(round(StudyS$True[1],3))
      MTDRes=paste(round(e$ResSim$pct.dose.rec[1],3))
      ToxRes=paste(round(e$ResSim$nb.tox.level[1]/e$ResSim$nsim,3))
      LevelRes=paste(round(e$ResSim$pat.level[1],3))
      for( i in 2 : length(StudyS$Prior))
      {
        PriorRes=paste(PriorRes,paste(round(StudyS$Prior[i],3)),sep=", ")
        TrueRes=paste(TrueRes,paste(round(StudyS$True[i],3)),sep=", ")
        MTDRes=paste(MTDRes,paste(round(e$ResSim$pct.dose.rec[i],3)),sep=", ")
        LevelRes=paste(LevelRes,paste(round(e$ResSim$pat.level[i],3)),sep=", ")
        ToxRes=paste(ToxRes,paste(round(e$ResSim$nb.tox.level[i]/e$ResSim$nsim,3)),sep=", ")
      }

      NS<-tklabel(Rec.fr,text=StudyS$Name,bg="cornflower blue")
      tkgrid(NS, row=1, column=1,  sticky="w")
      tkconfigure(NS, font=PoliceEtude, foreground="white")

      Titre1<-tklabel(Rec.fr,text="Results of the simulation")
      tkgrid(Titre1, row=3, column=1)
      tkconfigure(Titre1, font=PoliceTitre)
      model <- switch (StudyS$Model, ht="hyperbolique tangent",
                       power="Power", logit1="1-logistic")
      aff_5.0<-tklabel(Rec.fr,text=paste("Number of dose levels "))
      aff_5.1<-tklabel(Rec.fr,text=paste(StudyS$Nb_Dl))
      aff_3.0<-tklabel(Rec.fr,text=paste("Prior toxicity rates" ))
      aff_3.1<-tklabel(Rec.fr,text=paste(PriorRes))
      aff_2.0<-tklabel(Rec.fr,text=paste("True toxicity rates" ))
      aff_2.1<-tklabel(Rec.fr,text=paste(TrueRes))
      aff_4.0<-tklabel(Rec.fr,text=paste("Target DLT rate"))
      aff_4.1<-tklabel(Rec.fr,text=paste(StudyS$Target))
      aff_1.0<-tklabel(Rec.fr,text=paste("Method "))
      aff_1.1<-tklabel(Rec.fr,text=paste("Time-to-Event Escalation With Overdose Control "))
      aff_6.0<-tklabel(Rec.fr,text=paste("Statistical model "))
      aff_6.1<-tklabel(Rec.fr,text=paste(model))
      aff_8.0<-tklabel(Rec.fr,text=paste("% recommendation MTD"))
      aff_8.1<-tklabel(Rec.fr,text=paste(MTDRes))
      aff_7.0<-tklabel(Rec.fr,text=paste("Sample size"))
      aff_7.1<-tklabel(Rec.fr,text=paste(e$ResSim$samplesize))
      aff_9.0<-tklabel(Rec.fr,text=paste("Percentage of patients treated by dose level"))
      aff_9.1<-tklabel(Rec.fr,text=paste(LevelRes))
      aff_10.0<-tklabel(Rec.fr,text=paste("Average number of toxicities observed by dose level"))
      aff_10.1<-tklabel(Rec.fr,text=paste(ToxRes))
      aff_11.0<-tklabel(Rec.fr,text=paste("Number of simulations"))
      aff_11.1<-tklabel(Rec.fr,text=paste(StudyS$Nsim))
      aff_12.0<-tklabel(Rec.fr,text=paste("Mean inter-patient arrival time"))
      aff_12.1<-tklabel(Rec.fr,text=paste(StudyS$Tmean.arrival))
      aff_13.0<-tklabel(Rec.fr,text=paste("Observation window"))
      aff_13.1<-tklabel(Rec.fr,text=paste(StudyS$Obs.wind))

      tkgrid(aff_1.0, row=4, column=1,  sticky="w")
      tkgrid(aff_1.1, row=4, column=2,  sticky="w")
      tkconfigure(aff_1.0, font=Policeligne)
      tkconfigure(aff_1.1, font=Policeligne , foreground="blue")

      tkgrid(aff_2.0, row=6, column=1, sticky="w")
      tkgrid(aff_2.1, row=6, column=2, sticky="w")
      tkconfigure(aff_2.0, font=Policeligne)
      tkconfigure(aff_2.1, font=Policeligne , foreground="blue")

      tkgrid(aff_3.0, row=7, column=1, sticky="w")
      tkgrid(aff_3.1, row=7, column=2,  sticky="w")
      tkconfigure(aff_3.0, font=Policeligne )
      tkconfigure(aff_3.1, font=Policeligne , foreground="blue")

      tkgrid(aff_4.0, row=8, column=1, sticky="w")
      tkgrid(aff_4.1, row=8, column=2,  sticky="w")
      tkconfigure(aff_4.0, font=Policeligne )
      tkconfigure(aff_4.1, font=Policeligne , foreground="blue")

      tkgrid(aff_5.0, row=9, column=1,  sticky="w")
      tkgrid(aff_5.1, row=9, column=2,sticky="w")
      tkconfigure(aff_5.0, font=Policeligne )
      tkconfigure(aff_5.1, font=Policeligne , foreground="blue")

      tkgrid(aff_6.0, row=10, column=1,  sticky="w")
      tkgrid(aff_6.1, row=10, column=2,  sticky="w")
      tkconfigure(aff_6.0, font=Policeligne)
      tkconfigure(aff_6.1, font=Policeligne , foreground="blue")

      tkgrid(aff_7.0, row=11, column=1,  sticky="w")
      tkgrid(aff_7.1, row=11, column=2,  sticky="w")
      tkconfigure(aff_7.0, font=Policeligne )
      tkconfigure(aff_7.1, font=Policeligne , foreground="blue")

      tkgrid(aff_11.0, row=12, column=1,  sticky="w")
      tkgrid(aff_11.1, row=12, column=2,  sticky="w")
      tkconfigure(aff_11.0, font=Policeligne )
      tkconfigure(aff_11.1, font=Policeligne , foreground="blue")

      tkgrid(aff_12.0, row=13, column=1,  sticky="w")
      tkgrid(aff_12.1, row=13, column=2,  sticky="w")
      tkconfigure(aff_12.0, font=Policeligne )
      tkconfigure(aff_12.1, font=Policeligne , foreground="blue")

      tkgrid(aff_13.0, row=14, column=1,  sticky="w")
      tkgrid(aff_13.1, row=14, column=2,  sticky="w")
      tkconfigure(aff_13.0, font=Policeligne )
      tkconfigure(aff_13.1, font=Policeligne , foreground="blue")

      tkgrid(aff_8.0, row=15, column=1,  sticky="w")
      tkgrid(aff_8.1, row=15, column=2,  sticky="w")
      tkconfigure(aff_8.0, font=Policeligne )
      tkconfigure(aff_8.1, font=Policeligne , foreground="blue")

      tkgrid(aff_9.0, row=16, column=1,  sticky="w")
      tkgrid(aff_9.1, row=16, column=2,  sticky="w")
      tkconfigure(aff_9.0, font=Policeligne )
      tkconfigure(aff_9.1, font=Policeligne , foreground="blue")

      tkgrid(aff_10.0, row=17, column=1,  sticky="w")
      tkgrid(aff_10.1, row=17, column=2,  sticky="w")
      tkconfigure(aff_10.0, font=Policeligne )
      tkconfigure(aff_10.1, font=Policeligne , foreground="blue")

      assign("Rec.save_", tkframe(Resultas_, borderwidth=3),envir = as.environment(pos))
      tkgrid(Rec.save_, row=20, column=0)

      l<-tklabel(Rec.save_,text="")
      tkgrid(l, row=2)
      ResExp_<-data.frame(cbind(StudyS$True,StudyS$Prior,round(e$ResSim$pct.dose.rec,3),
                                round(e$ResSim$pat.level,3),round(e$ResSim$nb.tox.level/e$ResSim$nsim,3)))
      colnames(ResExp_)<-c("True tox.", "Prior tox." ,"MTD dist.(%)",
                           "(%)patient","Av. tox. ")
      WordExp_=tkbutton(Rec.save_,text="Export Results",width=15,
                        command=function()ExcellExport(dframe=ResExp_),bg="cornflower blue")
      tkgrid(WordExp_, row=3)
      blank<-tklabel(Rec.save_,text="      ")
      tkgrid(blank, row=4)

      SaveSim_=tkbutton(Rec.save_,text="Save results",width=15,state="disabled",bg="cornflower blue")
      tkgrid(SaveSim_, row=5)

    }

    tkdestroy(Saisi_)
    tkdestroy(m2_)
    tkdestroy(Run_)

    assign("Saisi_", tkframe(Required_, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    assign("m2_", tkframe(Required_, relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2_, row=17, column=1, columnspan=4, rowspan=7)


    Nb_Niv_Doses <- StudyS$Nb_Dl
    for ( i in 1: Nb_Niv_Doses)
    {
      r=17 + i
      n.iter <- tclVar (StudyS$Prior[i])
      n.itert<- tclVar (StudyS$True[i])

      tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
      tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8, textvariable=n.itert,state="disabled"),envir = as.environment(pos)),row=r,column=3)

      tkgrid(assign(paste('labp',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=4)
      tkgrid(assign(paste('valp',i,sep=''),tkentry(m2_, width=8, textvariable=n.iter,state="disabled"),envir = as.environment(pos)),row=r,column=5)
    }

    SliderValue_=tclVar(StudyS$Nb_Dl)
    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",state="disabled")
    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=2, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Target)
    Target_<-tkentry(Saisi_, width=5,textvariable=n.iter ,state="disabled")
    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Nsim)
    Nsim_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level level",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Strt.dose)
    X0_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tmod_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    n.iter <- switch (StudyS$Model, ht="Hyperbolique tangent",
                      power="Power", logit1="1-Logistic")
    Model_ <- tk2spinbox(Saisi_, values = c(n.iter,"") ,state="disabled")
    tkgrid(Tmod_ , row=6,column=0, sticky="w")
    tkgrid(Model_, row=6,column=1, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept", font=PoliceGenerale)
    intcpt <- if (is.null(StudyS$Intcpt))
    { c("NA")  }
    else { StudyS$Intcpt}
    n.intcpt <- tclVar(intcpt)
    assign("intcpt_", tkentry(Saisi_, width=10,textvariable=n.intcpt, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt_, row=6,column=2, sticky="w")
    tkgrid(intcpt_, row=6,column=3, sticky="w")


    Tstop_=tklabel(Saisi_,text="At the moment an unique stopping rule is proposed: sample size                         ",
                   font=PoliceGenerale)
    tkgrid(Tstop_, row=7,column=0,columnspan=4, sticky="w")

    Tnum_=tklabel(Saisi_,text="Number of patients",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Npat)
    N_<-tkentry(Saisi_,width=5,textvariable=n.iter ,state="disabled")
    tkgrid(Tnum_, row=8, column=0, sticky="w")
    tkgrid(N_, row=8, column=1, sticky="w")

    Tpdist_ = tklabel(Saisi_,text="Prior alpha dist. ",font=PoliceGenerale)
    n.iter <- switch (StudyS$P.dist, unif="Uniform [p1,p2]",
                      gamma="Gamma (p1,p2)")
    Pdist_ <- tk2spinbox(Saisi_, values = c(n.iter,""), width=15, state="disabled")
    tkgrid(Tpdist_ , row=9,column=0, columnspan=2, sticky="w")
    tkgrid(Pdist_, row=9,column=1, sticky="w")

    Talpha_=tklabel(Saisi_,text="Prior alpha value (DFLT=1)", font=PoliceGenerale)
    Tp1_=tklabel(Saisi_,text="p1",font=PoliceGenerale)
    Tp2_=tklabel(Saisi_,text="p2",font=PoliceGenerale)
    n.al <- tclVar(StudyS$Alpha); n.p1 <- tclVar(StudyS$P1)
    n.p2 <- tclVar(StudyS$P2)
    alpha_<-tkentry(Saisi_, width=7, textvariable=n.al, state="disabled")
    p1_<-tkentry(Saisi_, width=7, textvariable=n.p1, state="disabled")
    p2_<-tkentry(Saisi_, width=7, textvariable=n.p2, state="disabled")
    tkgrid(Tp1_, row=10, column=0, sticky="w")
    tkgrid(p1_, row=10, column=1, sticky="w")
    tkgrid(Tp2_, row=10, column=2, sticky="w")
    tkgrid(p2_, row=10, column=3, sticky="w")
    tkgrid(Talpha_, row=11, column=0, columnspan=3,sticky="w")
    tkgrid(alpha_, row=11, column=2, sticky="w")


    Tpoint_=tklabel(Saisi_,text="Pointest in [0, 0.5]",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Pointest)
    point_<-tkentry(Saisi_, width=10, textvariable=n.iter ,state="disabled")
    tkgrid(Tpoint_, row=12, column=0, columnspan=2, sticky="w")
    tkgrid(point_, row=12, column=1, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_, state="disabled") ;
    const2_ <- tkradiobutton(Saisi_, state="disabled")
    rbValue.const_ <- tclVar(as.numeric(StudyS$Constrain))
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=12, column=2, columnspan=2, sticky="w")
    tkgrid(const1_, row=12,  column=4, sticky="w")
    tkgrid(const2_, row=13,column=4, sticky="w")

    Twind_<-tklabel(Saisi_, text="Observation window",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Obs.wind)
    Wind_<-tkentry(Saisi_,textvariable=n.iter, width=10,state="disabled")
    tkgrid(Twind_, row=13,column=0,columnspan=2, sticky="w")
    tkgrid(Wind_, row=13,column=1, sticky="w")

    Tparr_<-tklabel(Saisi_, text="Mean inter-patient arrival time",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Tmean.arrival)
    Parr_<-tkentry(Saisi_,textvariable=n.iter, width=10,state="disabled")
    tkgrid(Tparr_, row=14,column=0,columnspan=3, sticky="w")
    tkgrid(Parr_, row=14,column=2, sticky="w")

    Tacc_<-tklabel(Saisi_, text="Accrual",font=PoliceGenerale)
    Acc_<-tk2spinbox(Saisi_, values = c(StudyS$Accrual),width=7,state="disabled")
    tkgrid(Tacc_, row=15,column=0, sticky="w")
    tkgrid(Acc_, row=15,column=1, sticky="w")

    Ttox_<-tklabel(Saisi_, text="Time until toxicity occurence",font=PoliceGenerale)
    n.iter <- switch(StudyS$Toxocc, unif="uniform", exp="exponential")
    tox_<-tk2spinbox(Saisi_, values = c(n.iter), width=10, state="disabled")
    tkgrid(Ttox_, row=16,column=0,columnspan=3, sticky="w")
    tkgrid(tox_, row=16,column=2, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    n.iter <- tclVar(StudyS$Seed)
    Seed_<-tkentry(Saisi_, width=10,textvariable=n.iter ,state="disabled")
    tkgrid(Tseed_, row=17,column=0,columnspan=2, sticky="w")
    tkgrid(Seed_, row=17,column=2, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    Nam_<- tclVar(StudyS$Name)
    names.S_<-tkentry(Saisi_, width=20,textvariable=Nam_,state="disabled")
    tkgrid(labage_, row=18,column=0, sticky="w")
    tkgrid(names.S_, row=18,column=1,columnspan=2, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,state="disabled",bg="cornflower blue")
    tkgrid(RunSim_, row=1)

    .RecapSimB()
  }

  SimTEWOC=function()
  {

    tkdestroy(Win_TEWOC)
    frameInitSIM()

    .Crea.f_=function(...)
    {
      Nb_Niv_Doses_ <- as.numeric(tclvalue((SliderValue_)))

      for ( i in 1: Nb_Niv_Doses_)
      {
        r=17 + i

        tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=2)
        tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=3)

        tkgrid(assign(paste('labp',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale)),row=r,column=4)
        tkgrid(assign(paste('valp',i,sep=''),tkentry(m2_, width=8),envir = as.environment(pos)),row=r,column=5)

      }
      if( Nb_Niv_Doses_!=8)
      {
        for ( i in (Nb_Niv_Doses_+1):8)
        {
          r=17 + i
          tkgrid(assign(paste('labt',i,sep=''),tklabel(m2_, text=paste('True at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=2)
          tkgrid(assign(paste('valt',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=3)

          tkgrid(assign(paste('labp',i,sep=''),tklabel(m2_, text=paste('Prior at Level',i,sep=' '),font=PoliceGenerale,state="disabled")),row=r,column=4)
          tkgrid(assign(paste('valp',i,sep=''),tkentry(m2_, width=8,state="disabled"),envir = as.environment(pos)),row=r,column=5)

        }
      }
    }

    disp.int_=function()
    {
      if (!(tclvalue(tkget(Model_))=="1-Logistic"))
      {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale, state="disabled")
        assign("intcpt_", tkentry(Saisi_, width=10, state="disabled"),envir = as.environment(pos))
        tkgrid(Tintcpt_, row=6,column=2, sticky="w")
        tkgrid(intcpt_, row=6,column=3, sticky="w")
      }
      else {
        Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale)
        assign("intcpt_", tkentry(Saisi_, width=10),envir = as.environment(pos))
        tkgrid(Tintcpt_, row=6, column=2, sticky="w")
        tkgrid(intcpt_, row=6, column=3, sticky="w")
      }
    }
    .Simulation_=function (...)
    {
      .Sauvgarde.Study_=function(...)
      {
        Nb_Dl=as.numeric(tclvalue(SliderValue_))
        true=NULL
        prior=NULL

        if (Nb_Dl==2) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))))}
        if (Nb_Dl==3) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))))}
        if (Nb_Dl==4) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))),as.numeric(tclvalue(tkget(valp4))))}
        if (Nb_Dl==5) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))),as.numeric(tclvalue(tkget(valp4))),as.numeric(tclvalue(tkget(valp5))))}
        if (Nb_Dl==6) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))),as.numeric(tclvalue(tkget(valp4))),as.numeric(tclvalue(tkget(valp5))),as.numeric(tclvalue(tkget(valp6))))}
        if (Nb_Dl==7) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),
                              as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),as.numeric(tclvalue(tkget(valt7))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))),
                as.numeric(tclvalue(tkget(valp4))),as.numeric(tclvalue(tkget(valp5))),as.numeric(tclvalue(tkget(valp6))),as.numeric(tclvalue(tkget(valp7))))}
        if (Nb_Dl==8) {true=c(as.numeric(tclvalue(tkget(valt1))),as.numeric(tclvalue(tkget(valt2))),as.numeric(tclvalue(tkget(valt3))),
                              as.numeric(tclvalue(tkget(valt4))),as.numeric(tclvalue(tkget(valt5))),as.numeric(tclvalue(tkget(valt6))),
                              as.numeric(tclvalue(tkget(valt7))),as.numeric(tclvalue(tkget(valt8))))
        prior=c(as.numeric(tclvalue(tkget(valp1))),as.numeric(tclvalue(tkget(valp2))),as.numeric(tclvalue(tkget(valp3))),
                as.numeric(tclvalue(tkget(valp4))),as.numeric(tclvalue(tkget(valp5))),as.numeric(tclvalue(tkget(valp6))),
                as.numeric(tclvalue(tkget(valp7))),as.numeric(tclvalue(tkget(valp8))))}


        seed=NULL
        npat=NULL
        alpha=1
        target=as.numeric(tclvalue(tkget((Target_))))
        pointest=as.numeric(tclvalue(tkget(point_)))
        Const=as.logical(as.numeric(tclvalue((rbValue.const_))))
        model.=tclvalue(tkget(Model_))
        intcpt=NULL
        if (model. == "Hyperbolic tangent") model = "ht"
        if (model. == "1-Logistic") {model = "logit1"; intcpt=intcpt=as.numeric(tclvalue(tkget(intcpt_)))}
        if (model. == "Power") model = "power"

        toxocc <- switch(tclvalue(tkget(tox_)), uniform="unif", exponential="exp")
        pdist_=tclvalue(tkget(Pdist_))
        if (pdist_ == "Uniform [p1,p2]") pdist = "unif"
        if (pdist_ == "Gamma (p1,p2)") pdist = "gamma"
        accrual=tclvalue(tkget(Acc_))
        nsim=as.numeric(tclvalue(tkget((Nsim_))))
        x0=as.numeric(tclvalue(tkget((X0_))))
        npat=as.numeric(tclvalue(tkget((N_))))
        alpha=as.numeric(tclvalue(tkget((alpha_))))
        p2=as.numeric(tclvalue(tkget((p2_))))
        p1=as.numeric(tclvalue(tkget((p1_))))

        twind=as.numeric(tclvalue(tkget(Wind_)))
        tparr=as.numeric(tclvalue(tkget(Parr_)))
        seed=as.numeric(tclvalue(tkget((Seed_))))

        assign("Names_Study", tclvalue(tkget(names.S_)),envir = as.environment(pos))

        res.users <- 1
        if (target>0.55) cat("\n Warning: Target DLT rate too high")
        if (target<0.15) cat("\n Warning: Target DLT rate too low")
        if (npat <=0) stop('Number of patients to be enrolled <=0')
        if (npat > 100) cat("\n Warning: Number of patients to be enrolled > 100")

        if ((x0 <1)|(x0 > Nb_Dl)|(x0%%1 != 0)) stop(paste('Starting dose level incorrect, enter an integer between 1 and',Nb_Dl))
        if ((pointest < 0) | (pointest > 0.5)) stop('Pointest parameter must be within [0,0.5]')
        if (twind <= 0 ) stop('Observation window incorrect')
        if (tparr <= 0 ) stop('Mean inter patient arrival time must be strictly positive')

        if (nsim < 100) cat("\n Warning: Low number of simulations")
        check_nsim <- function (){
          res.users <- 2
          while (res.users != 0 & res.users != 1) {
            cat("\n Warning: Large number of simulations, continue?  y or n")
            yn <- readLines(n=1)
            if(yn == "y" | yn == "Y" | yn== "yes" | yn == "Yes"){res.users <- 1;}
            if(yn == "n" | yn == "N" | yn== "no" | yn == "No"){res.users <- 0;}
          }
          return(res.users)
        }

        if (any(prior <= 0) | any(prior >= 1)) stop('Prior probabilities must be within ]0,1[ ')
        if (is.unsorted(prior, strictly = TRUE)) stop('Prior probabilities should be monotonically increasing')
        if (any(true <= 0) | any(true >= 1)) stop('True probabilities must be within ]0,1[ ')
        if (is.unsorted(true, strictly = TRUE)) stop('True probabilities should be monotonically increasing')
        if (nsim > 10000)  {res.users <- check_nsim()}
        assign("StudyS", list(Name=paste(Names_Study),Nb_Dl=Nb_Dl, True=true, Prior=prior,Target=target,
                              P1=p1, P2=p2, Model=model, Accrual=accrual, Alpha=alpha, P.dist=pdist,
                              Method="rjags", Npat=npat, Pointest=pointest, Toxocc=toxocc, Intcpt=intcpt, Constrain=Const,
                              Nsim=nsim, Strt.dose=x0, Obs.wind=twind, Tmean.arrival=tparr, Seed=seed),envir = as.environment(pos))
        if(res.users == 1) {

          cat("\n Submission in progress... Please wait... ", "\n")

          e$ResSim <- .titeewocsim(nsim=StudyS$Nsim, true=StudyS$True, prior=StudyS$Prior, target=StudyS$Target,
                                   obswin = StudyS$Obs.wind, tparr = StudyS$Tmean.arrival, accrual = StudyS$Accrual,
                                   start = StudyS$Strt.dose, samplesize = StudyS$Npat, method = StudyS$Method,
                                   pointest=StudyS$Pointest, model = StudyS$Model, toxocc=StudyS$Toxocc,
                                   constrain = StudyS$Constrain, p1 = StudyS$P1, p2=StudyS$P2, p.dist=StudyS$P.dist,
                                   alphap = StudyS$Alpha, count= FALSE, intcpt=StudyS$Intcpt,
                                   seed=StudyS$Seed, burnin.itr = 2000, production.itr = 2000)

          .RecapSim()
          tk2notetab.select(net.TEWOC_, "Results")
        }
      }
      .Sauvgarde.Study_()

    }

    assign("Saisi_", tkframe(Required_, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)
    tkgrid(Saisi_, row=1, column=1, columnspan=4, rowspan=10)

    assign("Run_", tkframe(Required_, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)
    tkgrid(Run_ , row=24, column=6, sticky="e")

    SliderValue_ <- tclVar('8')

    slider_Nb_DL_ <- tkscale(Saisi_, from=2, to=8,showvalue=T, variable=SliderValue_, resolution=1, orient="horiz",command=.Crea.f_)

    tkgrid(tklabel(Saisi_,text="Number of dose levels",font=PoliceGenerale),row=1,column=0, sticky="w")
    tkgrid(slider_Nb_DL_ , row=1,column=2, sticky="w")

    Titre_=tklabel(Saisi_,text="Target DLT rate",font=PoliceGenerale)
    Target_<-tkentry(Saisi_, width=5)

    tkgrid(Titre_, row=2, column=0, sticky="w")
    tkgrid(Target_, row=2, column=1, sticky="w")

    Tnsim_<-tklabel(Saisi_, text="Number of simulations",font=PoliceGenerale)
    Nsim_<-tkentry(Saisi_, width=10)
    tkgrid(Tnsim_, row=4,column=0, sticky="w")
    tkgrid(Nsim_, row=4,column=1, sticky="w")

    TX0_<-tklabel(Saisi_, text="Starting dose level",font=PoliceGenerale)
    X0_<-tkentry(Saisi_, width=10)
    tkgrid(TX0_, row=5,column=0, sticky="w")
    tkgrid(X0_, row=5,column=1, sticky="w")

    Tmod_=tklabel(Saisi_,text="Model",font=PoliceGenerale)
    Model_ <-tk2spinbox(Saisi_, values = c("Hyperbolic tangent","1-Logistic","Power"), command=disp.int_)
    tkgrid(Tmod_ , row=6,column=0, sticky="w")
    tkgrid(Model_, row=6,column=1, sticky="w")

    Tintcpt_<-tklabel(Saisi_, text="intercept",font=PoliceGenerale, state="disabled")
    assign("intcpt_", tkentry(Saisi_, width=10, state="disabled"),envir = as.environment(pos))
    tkgrid(Tintcpt_, row=6, column=2, sticky="w")
    tkgrid(intcpt_, row=6, column=3, sticky="w")

    Tstop_=tklabel(Saisi_,text="At the moment an unique stopping rule is proposed: sample size                       ",
                   font=PoliceGenerale)
    tkgrid(Tstop_, row=7,column=0,columnspan=4, sticky="w")

    Tnum_=tklabel(Saisi_,text="Number of patients",font=PoliceGenerale)
    N_<-tkentry(Saisi_, width=5)
    tkgrid(Tnum_, row=8, column=0, sticky="w")
    tkgrid(N_, row=8, column=1, sticky="w")

    Tpdist_ = tklabel(Saisi_,text="Prior alpha dist. ",font=PoliceGenerale)
    Pdist_ <- tk2spinbox(Saisi_, values = c("Uniform [p1,p2]","Gamma (p1,p2)"),width=15)
    tkgrid(Tpdist_ , row=9,column=0, columnspan=2, sticky="w")
    tkgrid(Pdist_, row=9,column=1, sticky="w")

    Talpha_=tklabel(Saisi_,text="Prior alpha value (DFLT=1)", font=PoliceGenerale)
    Tp1_=tklabel(Saisi_,text="p1",font=PoliceGenerale)
    Tp2_=tklabel(Saisi_,text="p2",font=PoliceGenerale)
    alpha_<-tkentry(Saisi_, width=7)
    p1_<-tkentry(Saisi_, width=7);p2_<-tkentry(Saisi_, width=7)

    tkgrid(Tp1_, row=10, column=0, sticky="w")
    tkgrid(p1_, row=10, column=1, sticky="w")
    tkgrid(Tp2_, row=10, column=2, sticky="w")
    tkgrid(p2_, row=10, column=3, sticky="w")
    tkgrid(Talpha_, row=11, column=0, columnspan=3,sticky="w")
    tkgrid(alpha_, row=11, column=2, sticky="w")


    Tpoint_=tklabel(Saisi_,text="Pointest in [0, 0.5]",font=PoliceGenerale)
    point_<-tkentry(Saisi_, width=10)
    tkgrid(Tpoint_, row=12, column=0, columnspan=2, sticky="w")
    tkgrid(point_, row=12, column=1, sticky="w")

    Titrec_=tklabel(Saisi_,text="Dose skipping constraint",font=PoliceGenerale)
    const1_ <- tkradiobutton(Saisi_) ;
    const2_ <- tkradiobutton(Saisi_)
    rbValue.const_ <- tclVar(1)
    tkconfigure(const1_,variable=rbValue.const_,value=1, text="TRUE",font=PoliceGenerale)
    tkconfigure(const2_,variable=rbValue.const_,value=0, text="FALSE",font=PoliceGenerale)
    tkgrid(Titrec_, row=12, column=2, columnspan=2, sticky="w")
    tkgrid(const1_, row=12,  column=4, sticky="w")
    tkgrid(const2_, row=13,column=4, sticky="w")

    Twind_<-tklabel(Saisi_, text="Observation window",font=PoliceGenerale)
    Wind_<-tkentry(Saisi_, width=10)
    tkgrid(Twind_, row=13,column=0,columnspan=2, sticky="w")
    tkgrid(Wind_, row=13,column=1, sticky="w")

    Tparr_<-tklabel(Saisi_, text="Mean inter-patient arrival time",font=PoliceGenerale)
    Parr_<-tkentry(Saisi_, width=10)
    tkgrid(Tparr_, row=14,column=0,columnspan=3, sticky="w")
    tkgrid(Parr_, row=14,column=2, sticky="w")

    Tacc_<-tklabel(Saisi_, text="Accrual",font=PoliceGenerale)
    Acc_<- tk2spinbox(Saisi_, values = c("fixed","poisson"),width=7)
    tkgrid(Tacc_, row=15,column=0, sticky="w")
    tkgrid(Acc_, row=15,column=1, sticky="w")

    Ttox_<-tklabel(Saisi_, text="Time until toxicity occurence",font=PoliceGenerale)
    tox_<- tk2spinbox(Saisi_, values = c("uniform","exponential"), width=10)
    tkgrid(Ttox_, row=16,column=0,columnspan=3, sticky="w")
    tkgrid(tox_, row=16,column=2, sticky="w")

    Tseed_<-tklabel(Saisi_, text="Random number generator (seed)",font=PoliceGenerale)
    Seed_<-tkentry(Saisi_, width=10)
    tkgrid(Tseed_, row=17,column=0,columnspan=2, sticky="w")
    tkgrid(Seed_, row=17,column=2, sticky="w")

    labage_<-tklabel(Saisi_, text="Name of study ",font=PoliceGenerale)
    names.S_<-tkentry(Saisi_, width=20)

    tkgrid(labage_, row=18,column=0, sticky="w")
    tkgrid(names.S_, row=18,column=1,columnspan=2, sticky="w")

    RunSim_=tkbutton(Run_ ,text="Run",width=15,command=.Simulation_,bg="cornflower blue")
    tkgrid(RunSim_, row=1)

  }


  tclArrayVar <- function(Rarray=NULL)
  {
    if (!is.null(Rarray) && !is.vector(Rarray) && length(dim(Rarray))!=2)
      stop("Array must be one-dimensional or two-dimensional.")
    n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount +1L
    name <- paste("::RTcl", n,sep = "")
    l <- list(env = new.env(),nrow=0,ncol=0,ndim=0)
    assign(name, NULL, envir = l$env)
    reg.finalizer(l$env, function(env) tcl("unset", ls(env)))


    class(l) <- "tclArrayVar"
    if (is.null(Rarray))
    {
      ndim <- 2
      .Tcl(paste("set ",name,"(0,0) \"\"",sep=""))
    }
    else
    {
      if (is.vector(Rarray))
      {
        ndim <- 1
        Rarray <- as.data.frame(Rarray)
      }
      else
        ndim <- 2
      for (i in (1:nrow(Rarray)))
        if (ndim==2)
          for (j in (1:ncol(Rarray)))
            .Tcl(paste("set ",name,"(",i,",",j,") \"",paste(Rarray[i,j]),"\"",sep=""))
      else
        .Tcl(paste("set ",name,"(",i,",",1,") \"",paste(Rarray[i,1]),"\"",sep=""))
      if (!is.null(rownames(Rarray)))
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"",rownames(Rarray)[i],"\"",sep=""))
      else
        for (i in (1:nrow(Rarray)))
          .Tcl(paste("set ",name,"(",i,",",0,") \"\"",sep=""))
      if (!is.null(colnames(Rarray)))
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"",colnames(Rarray)[j],"\"",sep=""))
      else
        for (j in (1:ncol(Rarray)))
          .Tcl(paste("set ",name,"(",0,",",j,") \"\"",sep=""))
      l$nrow <- nrow(Rarray)
      l$ncol <- ncol(Rarray)
    }
    l$ndim <- ndim
    l
  }
  print.tclArrayVar <- function(tclArray,height=100,width=100)
  {
    tt <- tktoplevel()
    tclRequire("Tktable")
    tclArrayName <- ls(tclArray$env)
    table1 <- tkwidget(tt,"table",rows=paste(tclArray$nrow+1),cols=paste(tclArray$ncol+1),colwidth=15,titlerows="1",titlecols="1",
                       height=paste(height+1),width=paste(width+1))

    tkgrid(table1)
    tkconfigure(table1,variable=tclArrayName,background="white")
    tkconfigure(table1,resizeborders="col")
    tkconfigure(table1,multiline="0")
  }


  save_Qf2=function(){ tkmessageBox(title = "Data storage Infos",
                                    message = "Data are automatically saved after each inclusion", icon = "info", type = "ok")}
  save_Qf=function(){ tkmessageBox(title = "Data storage Infos",
                                   message = "Please use the button save study available in interactive TITE-EWOC or simulator to save your data", icon = "info", type = "ok")}

  about=function(){ tkmessageBox(title = "Information",
                                 message = "Time-to-Event Escalade With Overdose Control Interface 2017",
                                 icon = "info", type = "ok")}

  Open.help=function() {browseURL("https://www.ncbi.nlm.nih.gov/pubmed/21351289", browser=getOption("browser"),
                                  encodeIfNeeded = FALSE) }


  frameInit = function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_TEWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_TEWOC, "630x550")
    tkwm.geometry(Win_TEWOC, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TEWOC, ilogo)
    tktitle(Win_TEWOC) <- "GUI TEWOC"
    tkpack.propagate(Win_TEWOC, FALSE)

    topMenu <- tk2menu(Win_TEWOC)
    tkconfigure(Win_TEWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive Tite-EWOC", command=NewTEWOC)
    tkadd(newStudyMenu,"command", label="Tite-EWOC simulator", command=SimTEWOC)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_TEWOC); frameInit()} )
    tkadd(AideMenu,"command", label="Help R (Publi)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TEWOC))
  }

  frameInitTEWOC = function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_TEWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_TEWOC, "595x600")
    tkwm.geometry(Win_TEWOC, "+400+80")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TEWOC, ilogo)
    tktitle(Win_TEWOC) <- "GUI TEWOC"
    tkpack.propagate(Win_TEWOC, FALSE)

    topMenu <- tk2menu(Win_TEWOC)
    tkconfigure(Win_TEWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive Tite-EWOC", command=NewTEWOC)
    tkadd(newStudyMenu,"command", label="Tite-EWOC simulator", command=SimTEWOC)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_TEWOC); frameInit()})
    tkadd(AideMenu,"command", label="Help R (Publi)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf2)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TEWOC))


    assign("net.TEWOC", tk2notebook(Win_TEWOC, tabs = c("Input parameters","Include","Results")),
           envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.TEWOC , fill = "both", expand = 1)

    assign("Resultas", tk2notetab(net.TEWOC , "Results"),envir = as.environment(pos))
    tkpack.propagate(Resultas, FALSE)

    assign("include", tk2notetab(net.TEWOC , "Include"),envir = as.environment(pos))
    tkpack.propagate(include, FALSE)

    assign("Required", tk2notetab(net.TEWOC , "Input parameters"),envir = as.environment(pos))
    tkpack.propagate(Required, FALSE)

    assign("Saisi", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi, FALSE)

    assign("Run", tkframe(Required, relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run, FALSE)

    assign("m2", tkframe(Required, relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2, row=17, column=0, columnspan=2, rowspan=7)
  }

  frameInitSIM = function() {
    assign("PoliceGenerale", tkfont.create(family="calibri",underline=FALSE, size=12),envir = as.environment(pos))
    assign("PoliceTitre", tkfont.create(family="calibri",weight="bold",underline=TRUE, size=10),envir = as.environment(pos))
    assign("Win_TEWOC", tktoplevel(background = "light steel blue"),envir = as.environment(pos))
    tkwm.geometry(Win_TEWOC, "715x725")
    tkwm.geometry(Win_TEWOC, "+400+80")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(Win_TEWOC, ilogo)
    tktitle(Win_TEWOC) <- "GUI TEWOC"
    tkpack.propagate(Win_TEWOC, FALSE)

    topMenu <- tk2menu(Win_TEWOC)
    tkconfigure(Win_TEWOC, menu=topMenu)
    StudyMenu <- tk2menu(topMenu, tearoff=FALSE)
    AideMenu <- tk2menu(topMenu, tearoff=FALSE)
    QuitterMenu <- tk2menu(topMenu, tearoff=FALSE)

    tkadd(topMenu,"cascade", label="Study", menu=StudyMenu)
    tkadd(topMenu,"cascade", label="Help", menu=AideMenu)
    tkadd(topMenu,"cascade", label="Exit", menu=QuitterMenu)

    newStudyMenu <- tk2menu(StudyMenu, tearoff=FALSE)
    tkadd(StudyMenu,"cascade", label="New Study", menu=newStudyMenu)

    tkadd(newStudyMenu,"command", label="Interactive Tite-EWOC", command=NewTEWOC)
    tkadd(newStudyMenu,"command", label="Tite-EWOC simulator", command=SimTEWOC)

    tkadd(StudyMenu,"command", label="Open Study",command=loads)
    tkadd(StudyMenu,"command", label="Close Study",command=function(){ tkdestroy(Win_TEWOC); frameInit()})
    tkadd(AideMenu,"command", label="Help R (Publi)", command=Open.help)
    tkadd(AideMenu,"command", label="About", command=about)
    tkadd(QuitterMenu,"command", label="Save_Quit", command=save_Qf)
    tkadd(QuitterMenu,"command", label="No_Save_Quit", command=function() tkdestroy(Win_TEWOC))


    assign("net.TEWOC_", tk2notebook(Win_TEWOC, tabs = c("Input parameters","Results")),envir = as.environment(pos))

    tcl("ttk::style", "configure", "TNotebook.Tab",font=PoliceGenerale)
    tcl("ttk::style", "map", "TNotebook.Tab", background=c("active", "cornflower blue"))

    tkpack(net.TEWOC_ , fill = "both", expand = 1)

    assign("Resultas_", tk2notetab(net.TEWOC_ , "Results"),envir = as.environment(pos))
    tkpack.propagate(Resultas_, FALSE)

    assign("Required_", tk2notetab(net.TEWOC_ , "Input parameters"),envir = as.environment(pos))
    tkpack.propagate(Required_, FALSE)

    assign("Saisi_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Saisi_, FALSE)

    assign("Run_", tkframe(Required_,relief="groove",borderwidth=3),envir = as.environment(pos))
    tkpack.propagate(Run_, FALSE)

    assign("m2_", tkframe(Required_,relief="groove",borderwidth=2),envir = as.environment(pos))
    tkgrid(m2_, row=17, column=0, columnspan=2, rowspan=7)
  }

  frameInit()
}


.ewocht<- function (a, x, y, w, p1, p2,law)
{
  v=switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((((tanh(x[i])+1)/2)^a)^y[i]) * ((1 - w[i]*(((tanh(x[i])+1)/2)^a))^(1-y[i]))
  }
  return(v)
}

.ewochte<-function (a, x, y, w, p1, p2, law)
{
  v=a*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((((tanh(x[i])+1)/2)^a)^y[i]) * ((1 - w[i]*(((tanh(x[i])+1)/2)^a))^(1-y[i]))
  }
  return(v)
}
.ewochtv<-function (a, x, y, w, p1, p2, law)
{
  v=(a^2)*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((((tanh(x[i])+1)/2)^a)^y[i]) * ((1 - w[i]*(((tanh(x[i])+1)/2)^a))^(1-y[i]))
  }
  return(v)
}
.ewoclgt <-function (a, x, y, w, p1, p2, cst, law)
{
  v=switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * (plogis(cst+a*x[i])^y[i]) *
      ((1-w[i]*plogis(cst+a*x[i]))^(1-y[i]))
  }
  return(v)
}
.ewoclgte <-function (a, x, y, w, p1, p2, cst, law)
{
  v=a*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * (plogis(cst+a*x[i])^y[i]) *
      ((1-w[i]*plogis(cst+a*x[i]))^(1-y[i]))
  }
  return(v)
}
.ewoclgtv <-function (a, x, y, w, p1, p2, cst, law)
{
  v=(a^2)*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * (plogis(cst+a*x[i])^y[i]) *
      ((1-w[i]*plogis(cst+a*x[i]))^(1-y[i]))
  }
  return(v)
}
.ewocpw <-function (a, x, y, w, p1, p2, law)
{
  v=switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((x[i]^a)^y[i]) * ((1-w[i]*(x[i]^a))^(1-y[i]))
  }
  return(v)
}

.ewocpwe <-function (a, x, y, w, p1, p2, law)
{
  v=a*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((x[i]^a)^y[i]) * ((1-w[i]*(x[i]^a))^(1-y[i]))
  }
  return(v)
}
.ewocpwv <-function (a, x, y, w, p1, p2, law)
{
  v=(a^2)*switch(law,gamma=dgamma(a, shape = p1, scale = p2),unif=dunif(a,p1,p2))
  for (i in 1:length(x)) {
    v = v * ((x[i]^a)^y[i]) * ((1-w[i]*(x[i]^a))^(1-y[i]))
  }
  return(v)
}

.HTGamma<-function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*pow((exp(d[i])/(exp(d[i]) + exp(-d[i]))), alpha)
  }
  alpha ~ dgamma(p1, 1/p2)
}


.PowerGamma<-function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*pow(d[i], alpha)
  }
  alpha ~ dgamma(p1, 1/p2)
}

.LogisticGamma<-function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*ilogit(intcpt + alpha * d[i])
  }
  alpha ~ dgamma(p1, 1/p2)
}

.HTUnif <- function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*pow((exp(d[i])/(exp(d[i]) + exp(-d[i]))), alpha)

  }
  alpha ~ dunif(p1, p2)
}

.LogisticUnif <- function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*ilogit(intcpt + alpha * d[i])
  }
  alpha ~ dunif(p1, p2)
}

.PowerUnif <- function ()
{
  for (i in 1:N1) {
    s[i] ~ dbin(p[i], n[i])
    p[i] <- w[i]*pow(d[i], alpha)
  }
  alpha ~ dunif(p1, p2)
}

.titeewoc <- function(prior, target, dose=NULL, tox=NULL, level, n = length(level), weights = NULL, followup = NULL,
                      obswin = NULL, method = "exact", model = "ht", constrain = TRUE, pointest="plugin", p.dist="unif",
                      p1 = ifelse(p.dist == "gamma", 2, 0), p2=ifelse(p.dist == "gamma", 0.7, 5), alphap=1,
                      intcpt = 3,  seed=NULL, burnin.itr = 2000, production.itr = 2000)
{
  if (is.null(weights)) {
    if (!(is.null(followup) | is.null(obswin))) {
      weights <- followup/obswin
    } else {
      stop("Cannot define weigths, check followup and obswin")
    }
  } else { weigths <- pmin(weights, 1)}

  if (any(weights > 1) | any(weights < 0))
    stop(" Weights have to be between 0 and 1!")
  if (!is.null(dose)) {
    if (!(length(dose) == length(prior)))
      stop(" Prior and dose must be of same lengths")
  }

  if (!(length(tox) == length(level) & length(tox) == length(weights)))
    stop(" tox, level, and weights must be of same lengths!")

  if (missing(prior))
    stop(" Prior proabilities are missing")
  if (any(prior > 1) | any(prior < 0))
    stop(" Prior probabilities have to be between 0 and 1!")
  if ((target > 1) | (target < 0))
    stop(" Target has to be between 0 and 1!")
  if ((!model %in% c("ht", "logit1", "power")))
    stop("model must be one of 'ht', 'logit1', 'power'")
  if ((!p.dist %in% c("unif", "gamma")))
    stop("Prior distribution must be one of 'unif', 'gamma'")
  if ((p.dist == "unif")& (p2<=p1))
    stop("invalid parameters p1 and p2")
  if ((!method %in% c("exact", "rjags")))
    stop ("unknown estimation method")
  if (!((pointest=="plugin")|((pointest <= 0.5)&(pointest > 0))))
    stop ("Pointest must be equal to 'plugin' or between ]0,0.5]")
  if (!(pointest=="plugin")&(method=="exact"))
    warning("Only pointest equals to plugin is available for exact method")
  if (!is.null(seed))
    set.seed(seed)
  if (missing(level))
    stop(" Level is missing")

  weights[tox == 1] <- 1
  y1p <- tox
  w1p <- weights
  k <- length(prior)
  nb.pat.level <- rep(0,k)
  nb.tox.level <- rep(0,k)
  weights.level<- rep(0,k)
  for (i in 1:k){
    nb.pat.level[i]<-table(factor(level, levels = 1:k))[[i]]
    nb.tox.level[i]<-xtabs(tox~factor(level, levels = 1:k))[[i]]
    weights.level[i]<-xtabs(w1p~factor(level, levels = 1:k))[[i]]}
  inzero=which(nb.pat.level!=0)
  weights.level[inzero] <- weights.level[inzero]/nb.pat.level[inzero]
  if (method == "exact") {
    if (model == "ht") {
      dosescaled <- atanh(2*prior^(1/alphap)-1)
      x1p <- dosescaled[level]
      f.model <-function(dose, alpha,cst) {
        ((tanh(dose) + 1)/2)^alpha}

      cst.den <- integrate(.ewocht, 0, Inf, x1p, y1p, w1p, p1, p2, p.dist,
                           abs.tol = 0)[[1]]
      est <- integrate(.ewochte, 0, Inf, x1p, y1p, w1p, p1, p2, p.dist,
                       abs.tol = 0)[[1]]/cst.den
      cdf <- function(aU){ integrate(.ewocht, 0, aU, x1p, y1p, w1p, p1, p2, p.dist,
                                     abs.tol = 0)[[1]]/cst.den}
      ptox <- f.model(dosescaled,est,1)
      mtd. <- atanh(2*target^(1/est)-1)
    }

    if (model == "logit1") {
      dosescaled <- (qlogis(prior) - intcpt)/alphap
      x1p <- dosescaled[level]
      f.model <- function(dose, alpha,cst) {plogis(cst+alpha*dose)}

      cst.den <- integrate(.ewoclgt, 0, Inf, x1p, y1p, w1p,  p1, p2,
                           intcpt, p.dist, abs.tol = 0)[[1]]
      est <- integrate(.ewoclgte, 0, Inf, x1p, y1p, w1p,  p1, p2,
                       intcpt, p.dist, abs.tol = 0)[[1]]/cst.den
      cdf <- function(aU){ integrate(.ewoclgt, 0, aU, x1p, y1p, w1p,  p1, p2,
                                     intcpt, p.dist, abs.tol = 0)[[1]]/cst.den}
      ptox <- f.model(dosescaled,est,intcpt)
      mtd. <- (log(target/(1 - target)) - intcpt)/est
    }
    if (model == "power") {
      dosescaled <- prior^(1/alphap)
      x1p <- dosescaled[level]
      f.model <- function(dose, alpha, cst) {dose^alpha}

      cst.den <- integrate(.ewocpw, 0, Inf, x1p, y1p, w1p, p1, p2, p.dist,
                           abs.tol = 0)[[1]]
      est <- integrate(.ewocpwe, 0, Inf, x1p, y1p, w1p, p1, p2, p.dist,
                       abs.tol = 0)[[1]]/cst.den
      cdf <- function(aU){integrate(.ewocpw, 0, aU, x1p, y1p, w1p, p1, p2, p.dist,
                                    abs.tol = 0)[[1]]/cst.den}
      ptox <- f.model(dosescaled,est,1)
      mtd. <- target^(1/est)
    }

    fn <- function(aU_, q) {abs(cdf(aU_) - q)}

    alpha.quantiles <- sapply(c(0.025, 0.25, 0.5, 0.75, 0.975),
                              function(q) {
                                max.x <- seq(0, 10*est, length = 100)[which.min(sapply(seq(0,
                                                                                           10*est, length = 100), function(i) {
                                                                                             fn(i, q)
                                                                                           }))]
                                if(max.x==0){max.x <-0.001}
                                optimize(fn, c(0, max.x), q = q, tol = .Machine$double.eps^0.5)$minimum
                              })

    quantiles <- sapply(dosescaled, function(d) {
      f.model(d, sort(alpha.quantiles, TRUE),intcpt)
    })
    alpha.quantiles<- matrix(alpha.quantiles,nrow=1,ncol=5)
    rownames(quantiles) <- colnames(alpha.quantiles) <-
      c("2.5%", "25%", "50%", "75%", "97.5%")
    current <- level[length(level)]
    if (all(ptox <= target)) {
      dose.rec <- k
    } else if (all(ptox >= target)) {
      dose.rec <- 1
    } else {
      dose.rec <- if (!constrain) {
        which.min(abs(ptox - target))
      }else {
        which.min(abs(ptox[1:min(current + 1, k)] - target))
      }
    }
    results <- list(dose.rec = dose.rec, alpha.est = est, alpha.quantiles=alpha.quantiles,
                    post.ptox=ptox,quantiles.ptox = quantiles, prior=prior, target=target, model=model,
                    method=method, prior.alpha=list(dist=p.dist,p1=p1,p2=p2), sdose=dosescaled,
                    constrain=constrain, pointest=pointest, nb.pat.level=nb.pat.level,
                    nb.tox.level=nb.tox.level, weights.m.level=weights.level)

  }else{
    if (model == "ht"){
      model.file <- switch(p.dist,gamma=.HTGamma, unif=.HTUnif)
      f.dose<- function(alpha,ptox, cst) atanh(2*ptox^(1/alpha)-1)
      f.model <- function(dose, alpha,cst) {
        ((tanh(dose) + 1)/2)^alpha}
      dosescaled <- f.dose(alphap, prior, intcpt)
    } else if (model == "logit1") {
      model.file <- switch(p.dist,gamma=.LogisticGamma, unif=.LogisticUnif)
      f.dose<- function(alpha,ptox, cst)(qlogis(ptox) - cst)/alpha
      f.model <- function(dose, alpha,cst) {plogis(cst+alpha*dose)}
      dosescaled <- f.dose(alphap,prior,intcpt)
    } else {
      model.file <- switch(p.dist,gamma=.PowerGamma, unif=.PowerUnif)
      f.dose <- function(alpha,ptox, cst) ptox^(1/alpha)
      f.model <- function(dose, alpha, cst) {dose^alpha}
      dosescaled <- f.dose(alphap,prior,intcpt)
    }

    if (model == "logit1"){
      mydata <- list(N1 = k, s = nb.tox.level, w=weights.level, d = dosescaled, p1 = p1,
                     p2 = p2, intcpt=intcpt, n=nb.pat.level)
    }else {mydata <- list(N1 = k, s = nb.tox.level, w=weights.level, d = dosescaled,
                          p1 = p1, p2 = p2, n=nb.pat.level) }


    path.model <- file.path(tempdir(), "model.file.txt")
    R2WinBUGS::write.model(model.file, path.model)
    r.init <- switch(p.dist,gamma=rgamma(1,shape=p1,scale=p2),unif=runif(1, p1, p2))
    inits.list <- list(list(alpha = 1, .RNG.seed = sample(1:1e+06, size = 1),
                            .RNG.name = "base::Wichmann-Hill"),
                       list(alpha = r.init, .RNG.seed = sample(1:1e+06, size = 1), .RNG.name = "base::Wichmann-Hill"))

    jagsobj <- rjags::jags.model(path.model, data = mydata, n.chains = 2,
                                 quiet = TRUE, inits = inits.list)
    update(jagsobj, n.iter = burnin.itr, progress.bar = "none")
    j.samples <- rjags::jags.samples(jagsobj, "alpha", n.iter = production.itr/2,
                                     progress.bar = "none")
    alpha.samples <- c(j.samples$alpha)

    alpha.quantiles<-apply(as.matrix(alpha.samples),2,quantile, c(0.025, 0.25, 0.5,
                                                                  0.75, 0.975))

    samples.ptox  <-  sapply(dosescaled, function(d) {
      f.model(d, alpha.samples, intcpt)})
    quantiles <- apply(samples.ptox, 2, quantile, c(0.025, 0.25,
                                                    0.5, 0.75, 0.975))
    mean.alpha <- apply(as.matrix(alpha.samples), 2, mean)
    if (pointest=="plugin") {

      ptox <- sapply(dosescaled, function(d) {
        f.model(d, mean.alpha,intcpt)
      })
    } else { ptox <- NA}
    if (pointest!= "plugin") {
      mtd <- f.dose(alpha=alpha.samples,ptox = target,cst=intcpt)
      target.mtd <- quantile(mtd, pointest)
    }
    current <- level[length(level)]
    dose.rec <- if (pointest == "plugin") {
      if (!constrain) {
        which.min(abs(ptox - target))
      }else {
        which.min(abs(ptox[1:min(current + 1, k)] - target))
      }
    } else { if (!constrain) {
      which.min(abs(dosescaled - target.mtd))
    }else {
      which.min(abs(dosescaled[1:min(current + 1, k)] - target.mtd))
    }
    }
    results<-list(dose.rec = dose.rec, alpha.mean = mean.alpha, alpha.quantiles=alpha.quantiles,
                  post.ptox=ptox, quantiles.ptox = quantiles, prior=prior, target=target, model=model, mtd.est=target.mtd,
                  method=method, prior.alpha=list(dist=p.dist,p1=p1,p2=p2), sdose=dosescaled, constrain=constrain,
                  pointest=pointest, nb.pat.level=nb.pat.level, nb.tox.level=nb.tox.level,
                  weights.m.level=weights.level)
  }
  class(results) <- "Tite-Ewoc"
  return (results)
}

.titeewocsim <- function(nsim, true, prior, target, dose=NULL, accrual = "fixed", obswin = NULL, tparr = obswin,
                         start = 1, samplesize, method = "exact", pointest=0.25, model = "ht", toxocc="unif",
                         p.dist="unif", alphap = 1,  p1 = ifelse(p.dist == "gamma", 2, 0),
                         p2=ifelse(p.dist == "gamma", 0.7, 1), intcpt = 3, rateexp=1/obswin,
                         constrain = TRUE, count =FALSE, seed=NULL, burnin.itr = 2000, production.itr = 2000)
{

  if (missing(nsim))
    stop(" Number of simulations is missing")
  if (is.null(obswin))
    stop("Observation window must be defined")
  if (missing(prior))
    stop(" Prior proabilities are missing")
  if (any(prior > 1) | any(prior < 0))
    stop(" Prior probabilities have to be between 0 and 1!")
  if (missing(true))
    stop(" True proabilities are missing")
  if (any(true > 1) | any(true < 0))
    stop(" True probabilities have to be between 0 and 1!")
  if ((target > 1) | (target < 0))
    stop(" Target has to be between 0 and 1!")
  if ((!model %in% c("ht", "logit1", "power", "logit2")))
    stop("model must be one of 'ht', 'logit1', 'power' or 'logit2'")
  if ((!method %in% c("exact", "rjags")))
    stop ("unknown estimation method")
  if ((!accrual %in% c("fixed", "poisson")))
    stop ("unknown patient accrual scheme")
  if ((!toxocc %in% c("unif", "exp")))
    stop ("unknown distribution for time-to-toxicity")
  if (!((pointest=="plugin")|((pointest <= 0.5)&(pointest > 0))))
    stop ("Pointest must be equal to 'plugin' or between ]0,0.5]")
  if (!(pointest=="plugin")&(method=="exact"))
    warning("Only pointest equals to plugin is available for exact method")
  if (!is.null(seed))
    set.seed(seed)
  if (missing(samplesize))
    stop(" Sample size is missing")
  if (!is.null(dose)) {
    if (!(length(dose) == length(prior)))
      stop("Prior and dose must be of same lengths")
    if (start > (length(prior)))
      start <- min(which(start == dose))
  }
  if (!(length(prior) == length(true)))
    stop("Prior and true probabilities must be of same lengths")
  if (length(start) > 1)
    stop("Start must be a single value")

  k <- length(prior)
  dose.rec <-  DURATION <- rep(NA, nsim)
  alphaDist <- rep(NA, nsim)
  nb.pat.level <- matrix(NA,nrow=nsim,ncol=k)
  nb.tox.level <- matrix(NA,nrow=nsim,ncol=k)
  dose.rec.level <- numeric(k)
  sim <- 1
  for (sim in 1:nsim) {
    if (count) {
      cat("simulation number:", sim, "\n")
    }
    if (accrual == "fixed") {
      next.arrival <- rep(tparr,samplesize)
    }else if (accrual == "poisson") {
      next.arrival <- rpois(samplesize, tparr)
    }

    level <- rep(NA,samplesize)
    level[1]  <- start
    w <- y <- followuppat <- numeric(samplesize)
    if (toxocc == "unif")
      TItox <- round(runif(samplesize, 0, obswin))
    if (toxocc == "exp")
      TItox <- pmin(round(rexp(samplesize, rate=rateexp)),obswin)

    ttox <- t(matrix(rbinom(samplesize*k, 1, true),k,samplesize))
    for (i in 1:samplesize)
    {
      y[i] <- ttox[i,level[i]]
      if (y[i]==1) {
        followuppat[i] <- TItox[i]
        w[i] <- 1}
      for (j in 1:i) {
        if (y[j]==0){followuppat[j] <- min(followuppat[j]+ next.arrival[i],obswin)
        w[j]<-followuppat[j]/obswin }

      }
      w <- pmin(w,1)
      obj<-.titeewoc(prior=prior,target=target, dose=dose, obswin=obswin, tox=y[1:i],level=level[1:i], n = i,
                     weights = w[1:i], method = method, model = model, constrain = constrain, pointest=pointest,
                     p.dist=p.dist, p1 = p1, p2 = p2, intcpt = intcpt, alphap=alphap,
                     burnin.itr = burnin.itr, production.itr = production.itr)

      if (i < samplesize)
        level[i+1]<-obj$dose.rec

    }
    level.pat<-table(factor(level, levels = 1:k))
    level.tox<-xtabs(y~factor(level, levels = 1:k))
    for (l in 1:k){
      nb.pat.level[sim,l]<-level.pat[[l]]
      nb.tox.level[sim,l]<-level.tox[[l]]
    }
    dose.rec[sim] <- obj$dose.rec
    DURATION[sim] <- sum(next.arrival[1:(samplesize-1)])+followuppat[samplesize]


    if (method == "exact") {
      alphaDist[sim] <-obj$alpha.est
    } else {
      if (model == "logit2"){
        alphaDist[sim,] <-obj$alpha.mean}
      else { alphaDist[sim] <-obj$alpha.mean }
    }

  }
  for (l in 1:k){
    dose.rec.level[l]<-table(factor(dose.rec, levels = 1:k))[[l]]}
  dose.rec.level<-dose.rec.level/nsim*100
  sim.results<-list(pct.dose.rec = dose.rec.level,  prior=prior, true=true,
                    target=target, pat.level=colMeans(nb.pat.level), nb.tox.level=colSums(nb.tox.level),
                    duration=DURATION, alphaDist = alphaDist, method=method, samplesize=samplesize,
                    nsim=nsim, start=start, model=model)
  class(sim.results) <- "Tite-Ewoc.Sim"
  return (sim.results)
}




.print.titeewoc <- function(x)
{
  cat(" Estimation method: ", x$method, "\n")
  model.txt <- switch(x$model, ht = "Hyperbolic Tangent", logit1 = "1-parameter logistic",
                      power = "1-parameter power", logit2 = "Two-parameter logistic")
  cat("\n Model: ", model.txt, "\n")
  if (!(x$model == "logit2")) {
    cat("\n Prior.alpha:", x$prior.alpha[1],"", "Param1:", x$prior.alpha[2],
        "Param2:", x$prior.alpha[3], "\n")
  }

  dose.txt <- x$sdose
  cat("\n Standardised doses : \n")
  print(dose.txt)
  if (x$constrain) {
    cat("\n Modified (constrained) CRM used, starting dose level: ", "\n")
  } else {
    cat("\n Unmodified (unconstrained) CRM used \n")
  }
  if (x$pointest == "plugin") {
    cat("\n Plug-in estimate of probability of toxicity used to select next dose \n")
  } else {
    cat("\n", 100 * x$pointest, "percentile of (standardised) MTD distribution used to select next dose",
        "\n", 100 * x$pointest, "percentile is:", x$mtd.est,"\n")
  }

  tab <- rbind(x$nb.pat.level, x$nb.tox.level)
  rownames(tab) <- c("n_Patient", "n_Tox")
  colnames(tab) <- c(1:length(x$nb.tox.level))
  colnames(tab) <- paste0("dose_",colnames(tab))
  cat("\n Toxicities observed: \n")
  print(tab)

  tab2 <- signif(x$quantiles.ptox,3)
  colnames(tab2) <- colnames(tab)
  cat("\n Quantiles of posterior estimates of toxicity:: \n")
  print(tab2)

  cat("\n Next recommended dose level: ", x$dose.rec, "\n")
}


.print.titeewoc.sim <- function(x)
{
  cat("Operating characteristics based on", x$nsim , "simulations: ", "\n")

  cat("Starting dose level:", x$start , "\n")

  cat("Sample size:", x$samplesize, "\n")
  model.txt <- switch(x$model, ht = "Hyperbolic Tangent", logit1 = "1-parameter logistic",
                      power = "1-parameter power", logit2 = "Two-parameter logistic")
  cat("\n Model: ", model.txt, "\n")
  cat("\n Average study duration (mean [min,max]): ",
      mean(x$duration),"[",min(x$duration),",", max(x$duration),"]", "\n")
  tab <- signif(rbind(x$pct.dose.rec,x$pat.level),2)
  rownames(tab) <- c("Recommended dose pourcentage", "Proportion of patients treated")
  colnames(tab) <- c(1:length(x$pat.level))
  colnames(tab) <- paste0("dose_",colnames(tab))
  print(tab)
}

.plot.titeewoc <- function(x)
{
  if (class(x)=="Tite-Ewoc") {
    probs=t(x$quantiles.ptox)

    plot.data = data.frame(
      y2.5 = probs[,1],
      y25 = probs[,2],
      y50 = probs[,3],
      y75 = probs[,4],
      y97.5 = probs[,5],
      dose.level=c(1:dim(x$quantiles.ptox)[2]),
      target=x$target)
    plot.data$dose.level=factor(plot.data$dose.level)
    p <-ggplot2::ggplot(plot.data, aes(x=dose.level)) +
      ggplot2::geom_boxplot(
        aes(ymin = y2.5, lower = y25, middle = y50, upper = y75, ymax = y97.5,
            color=dose.level),
        stat = "identity"
      ) +  ggplot2::ggtitle("Posterior p(DLT) quantiles: 2.5%, 25%, 50%, 75%, 97.5%") +
      ggplot2::geom_hline(aes(yintercept = target),data = plot.data, col = 4, linetype = 2)
    return(p)
  } else {stop (" Plot defined only for Tite-Ewoc object") }
}


GUIP1 <- function()
{
  e <- new.env()

  welcome = function()
  {

    PoliceGenerale <- tkfont.create(family="calibri",underline=FALSE, size=14)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=20)
    e$Win_GUIP1=tktoplevel(background = "light steel blue")
    tkwm.geometry(e$Win_GUIP1, "765x450")
    tkwm.geometry(e$Win_GUIP1, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(e$Win_GUIP1, ilogo)
    tktitle(e$Win_GUIP1) <- "GUIP1"
    tkpack.propagate(e$Win_GUIP1, FALSE)

    e$wlc <- tkframe(e$Win_GUIP1,relief="groove",borderwidth=3)
    tkpack.propagate(e$wlc, FALSE)
    tkgrid(e$wlc, row=0, column=0, sticky="se")
    tkgrid(tklabel(e$wlc, text="Welcome to GUIP1",font=PoliceTitre), row=0,columnspan=1, column=2 )

    tkgrid(tklabel(e$wlc, text="    "), row=1, column=0)
    tkgrid(tklabel(e$wlc, text="To start select one of the following model-guided adaptive designs",font=PoliceGenerale),
           row=2, column=1,columnspan=3)
    tkgrid(tklabel(e$wlc, text="

                   "), row=3, column=0)
    CRMBbut=tkbutton(e$wlc, text="Bayesian-CRM", width=20, command=function(){dis.welcome();.CRMB(e=e)}, bg="cornflower blue", font=PoliceGenerale)
    tkgrid(CRMBbut, row=4, column=1)

    CRMLbut=tkbutton(e$wlc,text="CRM-Likelihood",width=20,command=function(){dis.welcome();.CRML(e=e)}, bg="cornflower blue", font=PoliceGenerale)
    tkgrid(CRMLbut, row=4, column=3)

    tkgrid(tklabel(e$wlc, text="


                   "), row=5, column=0)
    TITECRMbut=tkbutton(e$wlc,text="TITE-CRM", width=20, command=function(){dis.welcome();.TITECRM(e=e)}, bg="cornflower blue", font=PoliceGenerale)
    tkgrid(TITECRMbut, row=6, column=2)

    tkgrid(tklabel(e$wlc, text="


                   "), row=7, column=0)

    EWOCbut=tkbutton(e$wlc,text="EWOC", width=20, command=function(){dis.welcome();.EWOC(e=e)}, bg="cornflower blue", font=PoliceGenerale)
    tkgrid(EWOCbut, row=8, column=1)

    TITEEWOCbut=tkbutton(e$wlc,text="TITE-EWOC", width=20, command=function(){dis.welcome();.TEWOC(e=e)}, bg="cornflower blue", font=PoliceGenerale)
    tkgrid(TITEEWOCbut, row=8, column=3)

    tkgrid(tklabel(e$wlc, text="




                   "), row=9, column=4)

  }

  dis.welcome = function()
  {
    tkdestroy(e$Win_GUIP1)
    PoliceGenerale<- tkfont.create(family="calibri",underline=FALSE, size=14)
    PoliceTitre <- tkfont.create(family="calibri",weight="bold",underline=TRUE, size=20)
    e$Win_GUIP1=tktoplevel(background = "light steel blue")
    tkwm.geometry(e$Win_GUIP1, "765x459")
    tkwm.geometry(e$Win_GUIP1, "+400+180")
    ilogo <- tk2ico.load(file.path(Sys.getenv("R_HOME"), "bin", "R.exe"), res = "R")
    tk2ico.set(e$Win_GUIP1, ilogo)
    tktitle(e$Win_GUIP1) <- "GUIP1"
    tkpack.propagate(e$Win_GUIP1, FALSE)

    e$wlc <- tkframe(e$Win_GUIP1,relief="groove",borderwidth=3)
    tkpack.propagate(e$wlc, FALSE)
    tkgrid(e$wlc, row=0, column=0, sticky="se")
    tkgrid(tklabel(e$wlc, text="Welcome to GUIP1",font=PoliceTitre), row=0,columnspan=1, column=2 )

    tkgrid(tklabel(e$wlc, text="    "), row=1, column=0)
    tkgrid(tklabel(e$wlc, text="To launch a new model-guided adaptive design, please close ongoing
                   study and press refresh button",font=PoliceGenerale),
           row=2, column=1,columnspan=3)
    tkgrid(tklabel(e$wlc, text="

                   "), row=3, column=0)
    CRMBbut=tkbutton(e$wlc, text="Bayesian-CRM", width=20, state="disabled", bg="cornflower blue", font=PoliceGenerale)
    tkgrid(CRMBbut, row=4, column=1)

    CRMLbut=tkbutton(e$wlc,text="CRM-Likelihood",width=20,state="disabled", bg="cornflower blue", font=PoliceGenerale)
    tkgrid(CRMLbut, row=4, column=3)

    tkgrid(tklabel(e$wlc, text="


                   "), row=5, column=0)
    TITECRMbut=tkbutton(e$wlc,text="TITE-CRM", width=20, state="disabled", bg="cornflower blue", font=PoliceGenerale)
    tkgrid(TITECRMbut, row=6, column=2)

    tkgrid(tklabel(e$wlc, text="


                   "), row=7, column=0)

    EWOCbut=tkbutton(e$wlc,text="EWOC", width=20, state="disabled", bg="cornflower blue", font=PoliceGenerale)
    tkgrid(EWOCbut, row=8, column=1)

    TITEEWOCbut=tkbutton(e$wlc,text="TITE-EWOC", width=20, state="disabled", bg="cornflower blue", font=PoliceGenerale)
    tkgrid(TITEEWOCbut, row=8, column=3)

    tkgrid(tklabel(e$wlc, text="
                   "), row=9, column=4)


    TITECRMbut=tkbutton(e$wlc,text="Refresh", width=11, command=function(){tkdestroy(e$Win_GUIP1);GUIP1()}, bg="yellow", font=PoliceGenerale)
    tkgrid(TITECRMbut, row=10, column=2)

  }

  welcome()

}


