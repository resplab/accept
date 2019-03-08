model_run<-function(model_input=NULL)
{
  patient_data<-patients[1,]
  nms<-names(patient_data)

  if(length(model_input)>0)
  {
    patient_data[names(model_input)]<-model_input
  }
  else
    stop("Error: no input parameter was submitted")

  res<-predictACCEPT(patient_data)
  res<-res[-1:-(length(res)-12)]

  return(as.list(res))
}
