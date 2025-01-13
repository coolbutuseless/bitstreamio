

tab <- c("Parameter Name", "Type", "Value", "Comments", "forbidden_zero_bit", "u(1)", "0", "Despite being forbidden, it must be set to 0!", "nal_ref_idc", "u(2)", "3", "3 means it is “important” (this is an SPS).", "nal_unit_type", "u(5)", "7", "Indicates this is a sequence parameter set.", "profile_idc", "u(8)", "66", "Baseline profile.", "constraint_set0_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set1_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set2_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set3_flag", "u(1)", "0", "We’re not going to honor constraints.", "reserved_zero_4bits", "u(4)", "0", "Better set them to zero.", "level_idc", "u(8)", "10", "Level 1, sec A.3.1.", "seq_parameter_set_id", "ue(v)", "0", "We’ll just use id 0.", "log2_max_frame_num_minus4", "ue(v)", "0", "Let’s have as few frame numbers as possible.", "pic_order_cnt_type", "ue(v)", "0", "Keep things simple.", "log2_max_pic_order_cnt_lsb_minus4", "ue(v)", "0", "Fewer is better.", "num_ref_frames", "ue(v)", "0", "We will only send I slices.", "gaps_in_frame_num_value_allowed_flag", "u(1)", "0", "We will have no gaps.", "pic_width_in_mbs_minus_1", "ue(v)", "7", "SQCIF is 8 macroblocks wide.", "pic_height_in_map_units_minus_1", "ue(v)", "5", "SQCIF is 6 macroblocks high.", "frame_mbs_only_flag", "u(1)", "1", "We will not to field/frame encoding.", "direct_8x8_inference_flag", "u(1)", "0", "Used for B slices. We will not send B slices.", "frame_cropping_flag", "u(1)", "0", "We will not do frame cropping.", "vui_prameters_present_flag", "u(1)", "0", "We will not send VUI data.", "rbsp_stop_one_bit", "u(1)", "1", "Stop bit. I missed this at first and it caused me much trouble.")
tab <- matrix(tab, ncol = 4, byrow = TRUE) |> as.data.frame()
tab


# Example from here: https://www.cardinalpeak.com/blog/the-h-264-sequence-parameter-set
# This is an SPS header for an H264 video encoding



test_that("h264 pps encoding works", {
  
  # reference bytes from the blog author
  pps <- c(0x00, 0x00, 0x00, 0x01, 0x68, 0xce, 0x38, 0x80)
  pps_short <- pps[-(1:4)] |> as.raw()
  
  
  bs  <- bs_open(raw(), 'w')
  
  # Sect 7.3.1 NAL unit structure
  bs_write_bit(bs, F)                 # Forbidden zero bit. f(1)
  bs_write_uint(bs, 3, nbits = 2)     # nal_ref_idc.   u(2)
  bs_write_uint(bs, 8, nbits = 5)     # nal_unit_type. u(5). 7 = SPS, 8 = PPS (Table 7-1)
  
  # 7.3.2.2 Picture Parameter Set (PPS) Syntax
  bs_write_uint_exp_golomb(bs, 0) # ue(v) pic_parameter_set_id
  bs_write_uint_exp_golomb(bs, 0) # ue(v) seq_parameter_set_id
  bs_write_uint(bs, 0, nbits = 1) # u(1) entropy_coding_mode_flag
  bs_write_uint(bs, 0, nbits = 1) # u(1) bottom_field_pic_order_in_frame_present_flag 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_slice_groups_minus1 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_ref_idx_l0_default_active_minus1 
  bs_write_uint_exp_golomb(bs, 0) # ue(v) num_ref_idx_l1_default_active_minus1 
  bs_write_uint(bs, 0, nbits = 1) # u(1) weighted_pred_flag
  bs_write_uint(bs, 0, nbits = 2) # u(2) weighted_bipred_flag
  bs_write_sint_exp_golomb(bs, 0) # pic_init_qp_minus26 1 se(v)
  bs_write_sint_exp_golomb(bs, 0) # pic_init_qs_minus26 1 se(v)
  bs_write_sint_exp_golomb(bs, 0) # chroma_qp_index_offset 1 se(v)
  bs_write_uint(bs, 0, nbits = 1) # deblocking_filter_control_present_flag 1 u(1)
  bs_write_uint(bs, 0, nbits = 1) # constrained_intra_pred_flag 1 u(1)
  bs_write_uint(bs, 0, nbits = 1) # redundant_pic_cnt_present_flag 1 u(1)
  bs_write_uint(bs, 1, nbits = 1) # stop bit

  raw_vec <- bs_close(bs)
  raw_vec
  
  expect_identical(raw_vec, pps_short)
  
  
  bs  <- bs_open(raw_vec, 'r')
  
  expect_identical(bs_read_bit(bs, 1           ), FALSE)
  expect_identical(bs_read_uint(bs, 1, nbits = 2),  3L)
  expect_identical(bs_read_uint(bs, 1, nbits = 5),  8L)
  
  expect_identical(bs_read_uint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 2) , 0L)
  expect_identical(bs_read_sint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_sint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_sint_exp_golomb(bs) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 0L)
  expect_identical(bs_read_uint(bs, nbits = 1) , 1L)
  
  bs_close(bs)
})



