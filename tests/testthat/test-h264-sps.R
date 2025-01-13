

tab <- c("Parameter Name", "Type", "Value", "Comments", "forbidden_zero_bit", "u(1)", "0", "Despite being forbidden, it must be set to 0!", "nal_ref_idc", "u(2)", "3", "3 means it is “important” (this is an SPS).", "nal_unit_type", "u(5)", "7", "Indicates this is a sequence parameter set.", "profile_idc", "u(8)", "66", "Baseline profile.", "constraint_set0_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set1_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set2_flag", "u(1)", "0", "We’re not going to honor constraints.", "constraint_set3_flag", "u(1)", "0", "We’re not going to honor constraints.", "reserved_zero_4bits", "u(4)", "0", "Better set them to zero.", "level_idc", "u(8)", "10", "Level 1, sec A.3.1.", "seq_parameter_set_id", "ue(v)", "0", "We’ll just use id 0.", "log2_max_frame_num_minus4", "ue(v)", "0", "Let’s have as few frame numbers as possible.", "pic_order_cnt_type", "ue(v)", "0", "Keep things simple.", "log2_max_pic_order_cnt_lsb_minus4", "ue(v)", "0", "Fewer is better.", "num_ref_frames", "ue(v)", "0", "We will only send I slices.", "gaps_in_frame_num_value_allowed_flag", "u(1)", "0", "We will have no gaps.", "pic_width_in_mbs_minus_1", "ue(v)", "7", "SQCIF is 8 macroblocks wide.", "pic_height_in_map_units_minus_1", "ue(v)", "5", "SQCIF is 6 macroblocks high.", "frame_mbs_only_flag", "u(1)", "1", "We will not to field/frame encoding.", "direct_8x8_inference_flag", "u(1)", "0", "Used for B slices. We will not send B slices.", "frame_cropping_flag", "u(1)", "0", "We will not do frame cropping.", "vui_prameters_present_flag", "u(1)", "0", "We will not send VUI data.", "rbsp_stop_one_bit", "u(1)", "1", "Stop bit. I missed this at first and it caused me much trouble.")
tab <- matrix(tab, ncol = 4, byrow = TRUE) |> as.data.frame()
tab


# Example from here: https://www.cardinalpeak.com/blog/the-h-264-sequence-parameter-set
# This is an SPS header for an H264 video encoding


test_that("h264 sps encoding works", {

  # reference bytes from the blog author
  sps <- c(0x00, 0x00, 0x00, 0x01, 0x67, 0x42, 0x00, 0x0a, 0xf8, 0x41, 0xa2)
  sps_short <- sps[-(1:4)] |> as.raw()
  
  
  bs  <- bs_open(raw(), 'w')
  
  
  bs_write_bit(bs, F)               # Forbidden zero bit. f(1)
  bs_write_uint(bs, 3, nbits = 2)   # nal_ref_idc.   u(2)
  bs_write_uint(bs, 7, nbits = 5)   # nal_unit_type. u(5). 7 = SPS, 8 = PPS (Table 7-1)
  
  # Table 7.3.2.1.1 Sequence Parameter Set (SPS) data syntax
  bs_write_uint(bs, 66, nbits = 8)  # profile_idc u(66). Baseline profile
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set0_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set1_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set2_flag u(1)
  bs_write_uint(bs, 0, nbits = 1)   # constraint_set3_flag u(1)
  bs_write_uint(bs, 0, nbits = 4)   # reserved_zero_4bits u(4)
  bs_write_uint(bs, 10, nbits = 8)  # level_idx u(8)
  bs_write_uint_exp_golomb(bs, 0)   # seq_parameter_set_id ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # log2_max_frame_num_minus4 ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # pic_order_cnt_type ue(v)
  bs_write_uint_exp_golomb(bs, 0)   # log2_max_pic_order_cnt_lsb_minus4 ue(v) 
  bs_write_uint_exp_golomb(bs, 0)   # log2_max_pic_order_cnt_lsb_minus4 ue(v)
  bs_write_uint(bs, 0, nbits = 1)   # gaps_in_frame_num_value_allowed_flag  u(1)
  bs_write_uint_exp_golomb(bs, 7)   # pic_width_in_mbs_minus_1 ue(v)
  bs_write_uint_exp_golomb(bs, 5)   # pic_height_in_map_units_minus_1 ue(v) 
  bs_write_uint(bs, 1, nbits = 1)   # frame_mbs_only_flag
  bs_write_uint(bs, 0, nbits = 1)   # direct_8x8_inference_flag  u(1)
  bs_write_uint(bs, 0, nbits = 1)   # frame_cropping_flag  u(1)
  bs_write_uint(bs, 0, nbits = 1)   # vui_prameters_present_flag  u(1)
  bs_write_uint(bs, 1, nbits = 1)   # rbsp_stop_one_bit  u(1) 
  bs_align(bs, 8)
  
  
  raw_vec <- bs_close(bs)
  raw_vec
  
  expect_identical(raw_vec, sps_short)
  
  
  bs  <- bs_open(raw_vec, 'r')
  
  expect_identical(bs_read_bit(bs, 1           ), FALSE)
  expect_identical(bs_read_uint(bs, 1, nbits = 2),  3L)
  expect_identical(bs_read_uint(bs, 1, nbits = 5),  7L)
  
  expect_identical(bs_read_uint(bs, 1, nbits = 8), 66L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 4),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 8), 10L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  0L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  0L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  0L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  0L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  7L)
  expect_identical(bs_read_uint_exp_golomb(bs, 1),  5L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  1L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  0L)
  expect_identical(bs_read_uint(bs, 1, nbits = 1),  1L)
  
  bs_close(bs)
})



