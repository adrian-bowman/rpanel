#     Scottish Referendum results

d1 <- read.table('data/scottish_referendum_1.txt',
                 col.names = c('Council', 'Yes', 'No', 'Rejected', 'Total'))
d2 <- read.table('data/scottish_referendum_2.txt', col.names = c('turnout', 'Electorate'))
d3 <- read.table('data/scottish_referendum_3.txt',
                 col.names = c('council2', 'x1', 'r1', 'x2', 'r2',
                               'Scottish_only_identity', 'r3',
                               'Born_in_Scotland', 'r4',
                               'Unemployed', 'r5',
                               'Age_65_or_over', 'r6'))

all(d1$Council == d3$council2)

Scottish_referendum <- cbind(d1, d2[ , 'Electorate'],
                                 d3[ , c('Scottish_only_identity', 'Born_in_Scotland',
                                          'Unemployed', 'Age_65_or_over')])
names(Scottish_referendum)[6] <- 'Electorate'

save(Scottish_referendum, file = 'rpanel/data/Scottish_referendum.rda')
