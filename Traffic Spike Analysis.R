# Load data

traffic_spike_data <- read.csv("Traffic_Spike_minute.csv")
traffic_spike <- data.frame(traffic_spike_data)

# Rename variables

names(traffic_spike) <- c("timestamp","pageviews","shares","unique_visitors","video_views_30s","nugget_preview_ctr","events","pageviews_as_total","spike","pageview_change_same","share_change_same","visitor_change_same","video_change_same","np_ctr_change_same","event_change_same","pageviews_total_change_same", "pageview_change_before","share_change_before","visitor_change_before","video_change_before","np_ctr_change_before","event_change_before", "pageviews_total_change_before", "pageview_change_after","share_change_after","visitor_change_after","video_change_after","np_ctr_change_after","event_change_after", "pageviews_total_change_after")

# Recode variables

traffic_spike$timestamp <- as.character(traffic_spike$timestamp)
traffic_spike$spike <- factor(traffic_spike$spike, levels = c(0,1), labels = c("No Spike", "Spike"))


traffic_spike$pageview_change_same <- as.numeric(as.character(traffic_spike$pageview_change_same))
traffic_spike$share_change_same <- as.numeric(as.character(traffic_spike$share_change_same))
traffic_spike$video_change_same <- as.numeric(as.character(traffic_spike$video_change_same))
traffic_spike$np_ctr_change_same <- as.numeric(as.character(traffic_spike$np_ctr_change_same))
traffic_spike$pageviews_total_change_same <- as.numeric(as.character(traffic_spike$pageviews_total_change_same))

traffic_spike$pageview_change_before <- as.numeric(as.character(traffic_spike$pageview_change_before))
traffic_spike$share_change_before <- as.numeric(as.character(traffic_spike$share_change_before))
traffic_spike$video_change_before <- as.numeric(as.character(traffic_spike$video_change_before))
traffic_spike$np_ctr_change_before <- as.numeric(as.character(traffic_spike$np_ctr_change_before))
traffic_spike$pageviews_total_change_before <- as.numeric(as.character(traffic_spike$pageviews_total_change_before))

traffic_spike$pageview_change_after <- as.numeric(as.character(traffic_spike$pageview_change_after))
traffic_spike$share_change_after <- as.numeric(as.character(traffic_spike$share_change_after))
traffic_spike$video_change_after <- as.numeric(as.character(traffic_spike$video_change_after))
traffic_spike$np_ctr_change_after <- as.numeric(as.character(traffic_spike$np_ctr_change_after))
traffic_spike$pageviews_total_change_after <- as.numeric(as.character(traffic_spike$pageviews_total_change_after))

# Remove outliers

library("dplyr")

traffic_spike <- filter(traffic_spike, pageview_change_same < 500)
traffic_spike <- filter(traffic_spike, visitor_change_before < 1000)
traffic_spike <- filter(traffic_spike, video_change_same < 1000)
traffic_spike <- filter(traffic_spike, video_change_before < 1000)
traffic_spike <- filter(traffic_spike, video_change_after < 1000)

# t-tests

t_pageviews <- t.test(traffic_spike$pageviews ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_shares <- t.test(traffic_spike$shares ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_visitors <- t.test(traffic_spike$unique_visitors ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_video_views <- t.test(traffic_spike$video_views_30s ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_np_ctr <- t.test(traffic_spike$nugget_preview_ctr ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_pageviews_total <- t.test(traffic_spike$pageviews_as_total ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)

t_pageviews
t_shares
t_visitors
t_video_views
t_np_ctr
t_pageviews_total

t_pageview_change_same <- t.test(traffic_spike$pageview_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_share_change_same <- t.test(traffic_spike$share_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_visitor_change_same <- t.test(traffic_spike$visitor_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_video_change_same <- t.test(traffic_spike$video_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_np_ctr_change_same <- t.test(traffic_spike$np_ctr_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_pageviews_total_change_same <- t.test(traffic_spike$pageviews_total_change_same ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)

t_pageview_change_same
t_share_change_same
t_visitor_change_same
t_video_change_same
t_np_ctr_change_same
t_pageviews_total_change_same

t_pageview_change_before <- t.test(traffic_spike$pageview_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_share_change_before <- t.test(traffic_spike$share_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_visitor_change_before <- t.test(traffic_spike$visitor_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_video_change_before <- t.test(traffic_spike$video_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_np_ctr_change_before <- t.test(traffic_spike$np_ctr_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_pageviews_total_change_before <- t.test(traffic_spike$pageviews_total_change_before ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)

t_pageview_change_before
t_share_change_before
t_visitor_change_before
t_video_change_before
t_np_ctr_change_before
t_pageviews_total_change_before

t_pageview_change_after <- t.test(traffic_spike$pageview_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_share_change_after <- t.test(traffic_spike$share_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_visitor_change_after <- t.test(traffic_spike$visitor_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_video_change_after <- t.test(traffic_spike$video_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_np_ctr_change_after <- t.test(traffic_spike$np_ctr_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)
t_pageviews_total_change_after <- t.test(traffic_spike$pageviews_total_change_after ~ traffic_spike$spike, data = traffic_spike, na.rm = TRUE)

t_pageview_change_after
t_share_change_after
t_visitor_change_after
t_video_change_after
t_np_ctr_change_after
t_pageviews_total_change_after




library("stats")

p <- c(t_pageviews$p.value, t_shares$p.value, t_visitors$p.value, t_video_views$p.value, t_np_ctr$p.value, t_pageviews_total$p.value)
p.adjust(p1, method = "bonferroni")

p_same <- c(t_pageview_change_same$p.value, t_share_change_same$p.value, t_visitor_change_same$p.value, t_video_change_same$p.value, t_np_ctr_change_same$p.value, t_pageviews_total_change_same$p.value)
p.adjust(p_same, method = "bonferroni")

p_before <- c(t_pageview_change_before$p.value, t_share_change_before$p.value, t_visitor_change_before$p.value, t_video_change_before$p.value, t_np_ctr_change_before$p.value, t_pageviews_total_change_before$p.value)
p.adjust(p_before, method = "bonferroni")

p_after <- c(t_pageview_change_after$p.value, t_share_change_after$p.value, t_visitor_change_after$p.value, t_video_change_after$p.value, t_np_ctr_change_after$p.value, t_pageviews_total_change_after$p.value)
p.adjust(p_after, method = "bonferroni")


# Correlations

library("Hmisc")
library("dplyr")

matrix <- data.matrix(traffic_spike[2:8])

matrix_spike <- filter(traffic_spike, spike == "Spike")
matrix_spike <- filter(matrix_spike, pageview_change_same >= 50)
matrix_spike <- data.matrix(matrix_spike[2:8])

rcorr_all <- rcorr(matrix, type = "pearson")
rcorr_spike <- rcorr(matrix_spike, type = "pearson")

rcorr_all
rcorr_spike


matrix_change_same <- data.matrix(traffic_spike[10:16])

matrix_change_same_spike <- filter(traffic_spike, spike == "Spike")
matrix_change_same_spike <- filter(matrix_change_same_spike, pageview_change_same >= 50)
matrix_change_same_spike <- data.matrix(matrix_change_same_spike[10:16])

rcorr_change_same_all <- rcorr(matrix_change_same, type = "pearson")
rcorr_change_same_spike <- rcorr(matrix_change_same_spike, type = "pearson")

rcorr_change_same_all
rcorr_change_same_spike


matrix_change_before <- data.matrix(traffic_spike[,c(10,17:23)])

matrix_change_before_spike <- filter(traffic_spike, spike == "Spike")
matrix_change_before_spike <- filter(matrix_change_before_spike, pageview_change_same >= 50)
matrix_change_before_spike <- data.matrix(matrix_change_before_spike[,c(10,17:23)])

rcorr_change_before_all <- rcorr(matrix_change_before, type = "pearson")
rcorr_change_before_spike <- rcorr(matrix_change_before_spike, type = "pearson")

rcorr_change_before_all
rcorr_change_before_spike


matrix_change_after <- data.matrix(traffic_spike[,c(10,24:30)])

matrix_change_after_spike <- filter(traffic_spike, spike == "Spike")
matrix_change_after_spike <- filter(matrix_change_after_spike, pageview_change_same >= 50)
matrix_change_after_spike <- data.matrix(matrix_change_after_spike[,c(10,24:30)])

rcorr_change_after_all <- rcorr(matrix_change_after, type = "pearson")
rcorr_change_after_spike <- rcorr(matrix_change_after_spike, type = "pearson")

rcorr_change_after_all
rcorr_change_after_spike

# Regression Coefficients

traffic_spike_filter <- filter(traffic_spike, spike == "Spike")
traffic_spike_filter <- filter(traffic_spike, pageview_change_same >= 50)

lm_visitors_same <- lm(pageview_change_same ~ visitor_change_same, traffic_spike_filter)
lm_videos_same <- lm(pageview_change_same ~ video_change_same, traffic_spike_filter)

lm_visitors_same
lm_videos_same

# Scatterplots

library("ggplot")

traffic_spike_filter <- filter(traffic_spike, spike == "Spike")
traffic_spike_filter <- filter(traffic_spike, pageview_change_same >= 50)

splot_visitors_same <- ggplot(traffic_spike_filter, aes(visitor_change_same, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Unique Visitors") + ylab("% Change in Pageviews") +ggtitle("% Change in Unique Visitors vs. % Change in Pageviews")
splot_videos_same <- ggplot(traffic_spike_filter, aes(video_change_same, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Video Views (>= 30s)") + ylab("% Change in Pageviews") +ggtitle("% Change in Video Views (>= 30s) vs. % Change in Pageviews")
splot_visitors_same
splot_videos_same

splot_visitors_before <- ggplot(traffic_spike_filter, aes(visitor_change_before, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Unique Visitors (5m prior to spike)") + ylab("% Change in Pageviews (during spike)") +ggtitle("% Change in Unique Visitors (prior to spike) vs. % Change in Pageviews")
splot_videos_before <- ggplot(traffic_spike_filter, aes(video_change_before, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Video Views >= 30s (5m prior to spike)") + ylab("% Change in Pageviews (during spike)") +ggtitle("% Change in Video Views >= 30s (prior to spike) vs. % Change in Pageviews")
splot_visitors_before
splot_videos_before

splot_visitors_after <- ggplot(traffic_spike_filter, aes(visitor_change_after, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Unique Visitors (5m after spike)") + ylab("% Change in Pageviews (during spike)") +ggtitle("% Change in Unique Visitors (after spike) vs. % Change in Pageviews")
splot_videos_after <- ggplot(traffic_spike_filter, aes(video_change_after, pageview_change_same)) + geom_point(shape = 1) + geom_smooth(method = lm) + xlab("% Change in Video Views >= 30s (5m after spike)") + ylab("% Change in Pageviews (during spike)") +ggtitle("% Change in Video Views >= 30s (after spike) vs. % Change in Pageviews")
splot_visitors_after
splot_videos_after

library("GGally")

ggpairs(traffic_spike[,2:8])
ggpairs(traffic_spike[,10:15])
ggpairs(traffic_spike[,16:21])
ggpairs(traffic_spike[,22:27])
