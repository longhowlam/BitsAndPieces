###### sparklyr example code to perform FPGrowth algorithm ####################
library(sparklyr)
library(dplyr)
library(visNetwork)

#### spark connect ############################################################
sc <- spark_connect(master = "local")

#### first create some dummy data #############################################
transactions = readRDS("transactions.RDs")

#### upload to spark ##########################################################  
trx_tbl  = copy_to(sc, transactions, overwrite = TRUE)

#### The data needs to be aggregated per id and the items need to be in a list
trx_agg = trx_tbl %>% group_by(id) %>% summarise(items = collect_list(item))
trx_agg %>% count()


#### Helper functions #########################################################

#### Expose/call FPGrowth 

## The FPGrowth algorithm is not exposed yet in sparklyR so we need to invoke 
## it ourselves with the following helper function. It defines and fits the 
## algorithm given the data and minimum support and confidence

ml_fpgrowth = function(
  x, 
  features_col = "items",
  support,
  confidence
){
  ensure_scalar_character(features_col)
  ensure_scalar_double(support)
  ensure_scalar_double(confidence)
  
  sc = spark_connection(x)
  uid = sparklyr:::random_string("fpgrowth_")
  jobj = invoke_new(sc, "org.apache.spark.ml.fpm.FPGrowth", uid) 
  
  jobj %>% 
    invoke("setItemsCol", features_col ) %>%
    invoke("setMinConfidence", confidence) %>%
    invoke("setMinSupport", support)  %>%
    invoke("fit", spark_dataframe(x))
}


##### extract rules

# The nasty thing is that antecedent (LHS) and consequent (RHS) are lists
# We can split them and collect them to R

ml_fpgrowth_extract_rules = function(FPGmodel, nLHS = 1, nRHS = 1)
{
  rules = FPGmodel %>% invoke("associationRules")
  sdf_register(rules, "rules")
  
  exprs1 <- lapply(
    0:(nLHS - 1), 
    function(i) paste("CAST(antecedent[", i, "] AS string) AS LHSitem", i, sep="")
  )
  exprs2 <- lapply(
    0:(nRHS - 1), 
    function(i) paste("CAST(consequent[", i, "] AS string) AS RHSitem", i, sep="")
  )
  
  splittedLHS = rules %>% invoke("selectExpr", exprs1) 
  splittedRHS = rules %>% invoke("selectExpr", exprs2) 
  p1 = sdf_register(splittedLHS, "tmp1")
  p2 = sdf_register(splittedRHS, "tmp2")
  
  ## collecting output rules to R should be OK and not flooding R
  bind_cols(
    sdf_bind_cols(p1, p2) %>% collect(),
    rules %>% collect() %>% select(confidence)
  )
}


#### Plot resulting rules in a networkgraph

plot_rules = function(rules, LHS = "LHSitem0", RHS = "RHSitem0", cf = 0.3)
{
  rules = rules %>% filter(confidence > cf)
  nds = unique(
    c(
      rules[,LHS][[1]],
      rules[,RHS][[1]]
    )
  )
  
  nodes = data.frame(id = nds, label = nds, title = nds) %>% arrange(id)
  
  edges = data.frame(
    from =  rules[,LHS][[1]],
    to = rules[,RHS][[1]]
  )
  visNetwork(nodes, edges, main = "Groceries network", size=1) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visEdges(smooth = FALSE) %>%
    visPhysics(
      solver = "barnesHut", 
      forceAtlas2Based = list(gravitationalConstant = -20, maxVelocity = 1)
    )
}


###### example calls ##########################################################

FPGmodel = ml_fpgrowth(trx_agg, "items", support = 0.01, confidence = 0.01)

GroceryRules =  ml_fpgrowth(
  trx_agg,
  "items", 
  support = 0.01,
  confidence = 0.01
) %>%
  ml_fpgrowth_extract_rules(nLHS = 2, nRHS = 1)


plot_rules(GroceryRules)


##### disconnect from spark ###################################################

spark_disconnect(sc)





