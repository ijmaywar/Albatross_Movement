quantile(m_model$flaps,probs=.05)
quantile(m_model$wind_vel,probs=.05)
quantile(m_model$wind_vel,probs=.75)

# Wave slope soaring
WS_soaring <- m_model %>% filter(flaps<12,wind_vel<3.283906,GLS_state=='dry')

# Dynamic soaring
dynamic_soaring <- m_model %>% filter(flaps<12,wind_vel>11.6236,GLS_state=='dry')

unique(inner_join(WS_soaring, dynamic_soaring, by = "id")[,1])

# Choose a bird from the list printed above
bird <- "GHAL_20191218_Y239"

################################################################################
# Copy and paste selected datetimes from below into the MATLAB code

# Dynamic soaring for specific ID
m_model %>% filter(flaps<12,wind_vel>11.6236,GLS_state=='dry',id==bird)

# Wave slope soaring for specific ID
m_model %>% filter(flaps<12,wind_vel<3.283906,GLS_state=='dry',id==bird)






# # Dynamic soaring for this individual
# temp_dry <- m_model %>% filter(flaps<30,id=='BBAL_20200111_O767')
# temp_dry <- m_model %>% filter(flaps<12,wind_vel>11.6236,GLS_state=='dry',id=='BBAL_20200111_O767')
