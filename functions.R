library(tm)
library(quanteda)
library(tokenizers)
library(proxy)
library(DBI)
library(tidyverse)
library(ROC)


data_preparation = function(c, feature){
  
  if (feature == 'pos'){
    
    for (i in 1:length(c)) {
      
      #first remove word and only keep tag
      c[[i]] = gsub('\\S+?_(\\w+?)', '\\1', c[[i]])
      
      #then remove punctuation tag and only leave original punctuation
      c[[i]] = gsub('(.)_.', '\\1', c[[i]])
      
    }
    
  }
  if (feature == 'hyb'){
    
    for (i in 1:length(c)) {
      
      #first remove word only if nouns, verbs, and pronouns
      c[[i]] = gsub('\\w+?_(N\\w+?|V\\w+?|PRP\\$*)', '\\1', c[[i]])
      
      #then remove all other tags
      c[[i]] = gsub('(\\w+)_\\w+', '\\1', c[[i]])
      
      #finally remove punctuation tag and only leave original punctuation
      c[[i]] = gsub('(.)_.', '\\1', c[[i]])
      
    }
    
  }
  
  return(c)
  
}
tokenization = function(corpus, feature){
  
  if (feature == 'chars'){
    
    tok = tokens(corpus, what = 'fasterword') %>%
      tokens(remove_separators = T, remove_url = T, remove_symbols = T, 
             remove_numbers = T)
    
  }else{
    
    tok = tokenize_words(corpus, lowercase = T, strip_punct = F, strip_numeric = T) %>%
      tokens(remove_separators = T, remove_url = T, remove_symbols = T)
    
  }
  
  return(tok)
  
}
filtering = function(tokens, f){
  
  if (f > 0){
    
    d = dfm(tokens)
    
    features.to.remove = round(f*d@Dim[2])
    
    d %>% 
      dfm_tfidf %>%
      topfeatures(n = features.to.remove) -> removed.feat
    
    t.r = tokens_remove(tokens, names(removed.feat), valuetype = 'fixed')
    
  }else{
    
    t.r = tokens
    
  }
  
  return(t.r)
  
}
random_sample = function(tokens, words){
  
  v = vector(mode = 'list', length = length(names(tokens)))
  
  for (i in 1:length(tokens)) {
    
    range = length(tokens[[i]]) - words
    random.seed = sample(range, size = 1)
    
    names(v)[i] = names(tokens)[i]
    
    if (random.seed > 0){
      
      v[[i]] = tokens[[i]][random.seed:(random.seed+words-1)]
      
    }else{
      
      v[[i]] = tokens[[i]]
      
    }
    
  }
  
  v = tokens(v)
  
  return(v)
  
}
break_into_features = function(tok, feature, n){
  
  if (grepl('words|pos|hyb', feature) & n > 1){
    
    tok %>%
      tokens_ngrams(n = n, skip = 0, concatenator = "_") -> t.f
    
  }else if (feature == 'chars'){
    
    tok %>%
      lapply(paste, sep = ' ', collapse = ' ') %>%
      tokenize_character_shingles(n = n, lowercase = T, strip_non_alphanum = F) %>%
      tokens() -> t.f
    
  }else{
    
    t.f = tok
    
  }
  
  return(t.f)
  
}
combine.and.make.matrix = function(t, q.s, feature, n){
  
  t = c(q.s, t)
  
  t %>% 
    break_into_features(feature, n) %>% 
    dfm() -> d
  
  return(d)
  
}
test = function(matrix){
  
  results = data.frame(stringsAsFactors = F)
  
  matrix %>% 
    dfm_weight(scheme = 'boolean') -> matrix
  
  for (i in 2:nrow(matrix)) {
    
    q = matrix[1,]
    k = matrix[i,]
    
    docvars(q, field = 'authors') = gsub('(\\w+)_\\w+\\.txt', '\\1', docid(q))
    
    a = as.double(suppressMessages(length(q[q & k])))
    b = as.double(suppressMessages(length(q[q == 1])) - a)
    c = as.double(suppressMessages(length(k[k == 1])) - a)
    p = as.double(ncol(matrix))
    d = as.double(p - (a+b+c))
    
    results[i-1, 'matching'] = round((a+d)/p, 4)
    results[i-1, 'simpson'] = round(a/(a+b), 4)
    results[i-1, 'phi'] = round((a*d - b*c)/sqrt((a+b)*(a+c)*(c+d)*(b+d)), 4)
    results[i-1, 'jaccard'] = round(a/(a+b+c), 4)
    results[i-1, 'rao'] = round(a/p, 4)
    results[i-1, 'sokal_sneath'] = round((a/sqrt((a+b)*(a+c)))*(d/sqrt((d+b)*(d+c))), 4)
    results[i-1, 'cohen'] = round((2*(a*d - b*c))/(((a+b)*(b+d))+((a+c)*(c+d))), 4)
    results[i-1, 'ochiai'] = round(a/sqrt((a+b)*(a+c)), 4)
    results[i-1, 'kulczynski'] = round(((a/(a+b))+(a/(a+c)))/2, 4)
    results[i-1, 'mountford'] = round(2*a/(a*b + a*c + 2*b*c), 4)
    results[i-1, 'rogot_goldberg'] = round((a/(2*a + b + c))+(d/(2*d + b +c)), 4)
    results[i-1, 'hawkins_dotson'] = round(((a/(a+b+c))+(d/(b+c+d)))/2, 4)
    results[i-1, 'sorgenfrei'] = round(a**2/((a+b)*(a+c)), 4)
    results[i-1, 'ct3'] = round(log(1+a)/log(1+p), 4)
    results[i-1, 'cole'] = round((a*d - b*c)/((a+b)*(b+d)), 4)
    results[i-1, 'ct5'] = round((log(1 + a*d) - log(1 + b*c))/log(1 + p**2/4), 4)
    results[i-1, 'gk'] = round((2 * min(a, d) - b - c)/(2 * min(a, d) + b + c), 4)
    results[i-1, 'yule'] = round((a*d - b*c)/(a*d + b*c), 4)
    
    
    if (docvars(q)[[1]] == docid(k)){
      
      results[i-1, 'author'] = 1
      
    }else{
      
      results[i-1, 'author'] = 0
      
    }
    
  }
  
  
  return(results)
  
}
cosine = function(m1, m2){
  
  tmp = rbind(m1, m2)
  
  sim = tmp/sqrt(rowSums(tmp * tmp))
  sim = sim %*% t(sim)
  
  return(sim)
  
}
test_num = function(matrix, results){
  
  matrix %>% 
    dfm_trim(min_termfreq = 3000, termfreq_type = "rank") %>% 
    dfm_weight(scheme = 'prop') %>% 
    scale -> matrix
  
  for (i in 2:nrow(matrix)) {
    
    q = matrix[1,]
    k = matrix[i,]
    
    sim = cosine(q, k)
    
    results[i-1, 'cosine'] = round(sim[1,2], 4)
    
  }
  
  
  matrix[matrix > 0.43] = 1
  matrix[matrix < -0.43] = -1
  matrix[(matrix != 1) & (matrix != -1)] = 0
  
  matrix = as.dfm(matrix)
  
  for (i in 2:nrow(matrix)) {
    
    q = matrix[1,]
    k = matrix[i,]
    
    sim = cosine(q, k)
    
    results[i-1, 'evert'] = round(sim[1,2], 4)
    
  }
  
  return(results)
  
}
find_rank = function(results){
  
  ranks = data.frame(stringsAsFactors = F)
  
  results[, ncol(results)+1] = rank(-results$simpson, ties.method = 'max')
  ranks[1, 'simpson'] = results[which(results$author == 1), 'simpson']
  ranks[1, 'simpson_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$phi, ties.method = 'max')
  ranks[1, 'phi'] = results[which(results$author == 1), 'phi']
  ranks[1, 'phi_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$jaccard, ties.method = 'max')
  ranks[1, 'jaccard'] = results[which(results$author == 1), 'jaccard']
  ranks[1, 'jaccard_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$ochiai, ties.method = 'max')
  ranks[1, 'ochiai'] = results[which(results$author == 1), 'ochiai']
  ranks[1, 'ochiai_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$matching, ties.method = 'max')
  ranks[1, 'matching'] = results[which(results$author == 1), 'matching']
  ranks[1, 'matching_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$rao, ties.method = 'max')
  ranks[1, 'rao'] = results[which(results$author == 1), 'rao']
  ranks[1, 'rao_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$sokal_sneath, ties.method = 'max')
  ranks[1, 'sokal_sneath'] = results[which(results$author == 1), 'sokal_sneath']
  ranks[1, 'sokal_sneath_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$cohen, ties.method = 'max')
  ranks[1, 'cohen'] = results[which(results$author == 1), 'cohen']
  ranks[1, 'cohen_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$kulczynski, ties.method = 'max')
  ranks[1, 'kulczynski'] = results[which(results$author == 1), 'kulczynski']
  ranks[1, 'kulczynski_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$mountford, ties.method = 'max')
  ranks[1, 'mountford'] = results[which(results$author == 1), 'mountford']
  ranks[1, 'mountford_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$rogot_goldberg, ties.method = 'max')
  ranks[1, 'rogot_goldberg'] = results[which(results$author == 1), 'rogot_goldberg']
  ranks[1, 'rogot_goldberg_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$hawkins_dotson, ties.method = 'max')
  ranks[1, 'hawkins_dotson'] = results[which(results$author == 1), 'hawkins_dotson']
  ranks[1, 'hawkins_dotson_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$sorgenfrei, ties.method = 'max')
  ranks[1, 'sorgenfrei'] = results[which(results$author == 1), 'sorgenfrei']
  ranks[1, 'sorgenfrei_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$ct3, ties.method = 'max')
  ranks[1, 'ct3'] = results[which(results$author == 1), 'ct3']
  ranks[1, 'ct3_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$cole, ties.method = 'max')
  ranks[1, 'cole'] = results[which(results$author == 1), 'cole']
  ranks[1, 'cole_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$ct5, ties.method = 'max')
  ranks[1, 'ct5'] = results[which(results$author == 1), 'ct5']
  ranks[1, 'ct5_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$gk, ties.method = 'max')
  ranks[1, 'gk'] = results[which(results$author == 1), 'gk']
  ranks[1, 'gk_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$yule, ties.method = 'max')
  ranks[1, 'yule'] = results[which(results$author == 1), 'yule']
  ranks[1, 'yule_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$evert, ties.method = 'max')
  ranks[1, 'evert'] = results[which(results$author == 1), 'evert']
  ranks[1, 'evert_rank'] = results[which(results$author == 1), ncol(results)]
  
  results[, ncol(results)+1] = rank(-results$cosine, ties.method = 'max')
  ranks[1, 'cosine'] = results[which(results$author == 1), 'cosine']
  ranks[1, 'cosine_rank'] = results[which(results$author == 1), ncol(results)]
  
  return(ranks)
  
}
test_coefficients = function(corpus, feature, n, q.sample, k.sample){
  
  #tokenisation
  SimpleCorpus(DirSource(corpus, pattern = '\\.txt$')) %>% 
    corpus() %>% 
    data_preparation(feature) %>%
    tokenization(feature) -> tok
  
  #saving author names
  docvars(tok, field = 'authors') = gsub('(\\w+)_\\w+\\.txt', '\\1', docid(tok))
  
  cat(paste(feature, n, sep = ' '), sep = '\n')
  
  #output table
  ranks = data.frame(stringsAsFactors = F) #this table will contain the rank of the correct author
  final.results = data.frame(stringsAsFactors = F) #this table will contain the values of the coefficients for same-author comparisons vs different-author comparisons
  
  #initialising progress bar
  pb = txtProgressBar(0, length(tok), 1, style = 3)
  
  for (i in 1:length(tok)) {
    
    q = tok[i]
    t = tok[-i]
    
    #taking a random sample for Q
    q %>% 
      random_sample(q.sample) -> q.samp
    
    #re-adding author sames
    docvars(q.samp, field = 'authors') = docvars(tok, field = 'authors')[i]
    docvars(t, field = 'authors') = docvars(tok, field = 'authors')[-i]
    
    #taking a random sample of K data and combining with Q in final matrix
    t %>%
      quanteda:::tokens_group(groups = docvars(t, field = 'authors')) %>% 
      random_sample(k.sample) %>% 
      combine.and.make.matrix(q.samp, feature, n) -> d
    
    #calculate binary coefficients
    d %>% test() -> results
    
    #calculate numeric coefficients
    d %>% 
      test_num(results) -> results
    
    final.results = bind_rows(final.results, results)
    
    results %>% find_rank() -> rank
    
    rank = as.data.frame(rank)
    rank[1, "Q"] = docnames(tok[i])
    
    ranks = bind_rows(ranks, rank)
    
    setTxtProgressBar(pb, i)
    
  }
  
  close(pb)
  
  return(results = list(coefficient_table = final.results, 
                        correct_author_rank = ranks))
  
}
calibrate.llr = function(background, test, coeff){
  
  #creating initial model using training data
  background %>% 
    mutate(target = case_when(author == 1 ~ TRUE,
                              author == 0 ~ FALSE)) %>% 
    rename(score = coeff) %>% 
    select(target, score) %>% 
    train.logreg() -> calibration.model
  
  #using recalibrated model to calculate llr in test data
  test %>% 
    mutate(target = case_when(author == 1 ~ TRUE,
                              author == 0 ~ FALSE)) %>% 
    rename(score = coeff) %>% 
    select(target, score) %>% 
    predict(calibration.model, newdata = .) -> llr
  
  table = select(test, coeff, author) %>% mutate(llr = log10(exp(llr)))
  
  #getting cllrs
  test %>% 
    cbind(llr) %>% 
    mutate(target = case_when(author == 1 ~ TRUE,
                              author == 0 ~ FALSE)) %>% 
    rename(score = llr) %>% 
    roc() %>% 
    summary.roc() -> cllr
  
  #plotting tippet
  test %>% 
    cbind(llr) %>%  
    mutate(target = case_when(author == 1 ~ TRUE,
                              author == 0 ~ FALSE)) %>% 
    rename(score = llr) %>% 
    roc() %>% 
    tippet.plot()
  tippet = recordPlot()
  
  results = list(stats = cllr, tippet = tippet, table = table)
  
  return(results)
  
}
extract_unique_ngrams = function(corpus, feature, n){
  
  SimpleCorpus(DirSource(corpus, pattern = '\\.txt$')) %>% 
    corpus() %>% 
    data_preparation(feature) %>%
    tokenization(feature) %>%
    filtering(0) -> tok
  
  docvars(tok, field = 'authors') = gsub('(\\w+)_\\w+\\.txt', '\\1', docid(tok))
  
  tok %>% 
    break_into_features(feature, n) %>% 
    dfm() -> d
  
  #finding 50 items that are used only by one author in more than one document
  d %>% 
    dfm_weight(scheme = 'boolean') %>% 
    dfm_group(groups = authors) %>% 
    dfm_trim(min_termfreq = 2, max_termfreq = 3, max_docfreq = 1) -> dtrim
  
  topfeatures(dtrim, n = 50, groups = authors) -> list
  
  return(list)
  
}






