library(httr)
library(openai)

response <-
  create_chat_completion(
  model = "gpt-3.5-turbo",
  messages = list(
    list(
      "role" = "system",
      "content" = "We have a predictive model, called ACCEPT that projects patient outcomes based on a number of characteristics. We would like to call this model in R. Here is an example: accept(tidyr::tibble(ID = 1, male = 1,age = 65,
    smoker = 0,
    oxygen = 0,
    statin = 1,
    LAMA = 1,
    LABA = 1,
    ICS = 0,
    FEV1 = 61,
    BMI = 25,
    SGRQ = 55,
    LastYrExacCount = 1,
    LastYrSevExacCount = 0,
    observedExacRate = 0.67,
    observedSevereExacRate = 0.33
))

Some of the inputs in the function call are patient characteristics. These include \`male\`, \`smoker\`, \`oxygen\`, 0, \`statin\`, \`LAMA\`, \`LABA\`,\`ICS\`, \`FEV1\` ,\`BMI\`, \"SGRQ\`, \`LastYrExacCount\` and \`LastYrSevExacCount\`. I will explain to you what each variable is. \`male\` is 1 when the patient is male, and 0 when the patient is female. \`smoker\` is 1 if the patient is a current smoker, and 0 if the patient is a former smoker or a non-smoker. \`oxygen\` is another binary variable representing whether the patient has received oxygen therapy at home in the last year. \`statin\`, \`LAMA\`, \`LABA\`, and \`ICS\` are medication variables, and are 1 if the patient is receiving any medications of that class, and 0 if the patient is not. FEV1 is represented as a percentage predicted value, and is between 0 and 100. St George\'s Respiratory Questionnaire (SGRQ) is a symptoms score, and ranges between 0 and 100. The SGRQ variable is flexible. If SGRQ is not provided, it is possible to submit either CAT score or mMRC score in the API call. However, between SGRQ, CAT, and mMRC, one should be submitted. \`LastYrExacCount\` is the total number of COPD exacerbations the patient had in the previous year, irrespective of their severity. \`LastYrSevExacCount\` is the number of severe exacerbations the patient had in the previous year.

Your task is to extract patient characteristics from a doctor\'s dictation and recreate the function call given above to call the model based on new patient characteristics. If a medication is a combination of two or more medication classes, you can assume the patient is receiving those medications. If a patient is a cardiovascular patient, you can assume they are on statins. If you don\'t understand any of the required patient characteristics, you will ask the doctor for clarification and the create and return the R command. If a predictor is missing, or if there is conflicting information in the dictation, you should ask the doctor a follow up question to clarify and then recreate the R command. Only return the R command with no explanations."
    ),
list(
  "role" = "user",
  "content" = "Patient is 55 years old female who is a former smoker with an FEV1 of 56%. The patient is receiving symbicort, and has received oxygen therapy in the past year and is a cardiac patient. The patient has a BMI of 21 and an SGRQ score of 55. The patient had 3 exacerbations in the last year, one of which was severe. "
)
  )
)

response$choices$message.content
