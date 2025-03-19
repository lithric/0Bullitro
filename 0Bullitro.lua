_G.FFI = require("ffi")

---@alias RGBA [number,number,number,number]

---@class G: Game
---@field debug_tools UIBox
---@field OVERLAY_MENU UIBox
---@field BRUTE_OVERLAY RGBA
_G.G = G

---@class Back: Object
---@operator call:Back
_G.Back = Back

---@class Game: Object
---@operator call:Game
_G.Game = Game

---@class Tag: Object
---@operator call:Tag
_G.Tag = Tag

---@class Particles: Moveable
---@operator call:Particles
_G.Particles = Particles

---@class Sprite: Moveable
---@operator call:Sprite
_G.Sprite = Sprite

---@class AnimatedSprite: Sprite
---@operator call:AnimatedSprite
_G.AnimatedSprite = AnimatedSprite

---@class DynaText: Moveable
---@operator call:DynaText
_G.DynaText = DynaText

---@class Blind: Moveable
---@operator call:Blind
_G.Blind = Blind

---@class Card_Character: Moveable
---@operator call:Card_Character
_G.Card_Character = Card_Character

---@class Card: Moveable
---@operator call:Card
_G.Card = Card


---@type {extra: GameObjectData}
Card.ability = Card.ability

---@class CardArea: Moveable
---@operator call:CardArea
_G.CardArea = CardArea

---@type Object[]
_G.EVENT_FUNCTIONS = {}

--------------------------------------------------------------------------------------------------------------------------
-- Decoder of GIF-files (not made by me)
--------------------------------------------------------------------------------------------------------------------------
-- This module extracts images from GIF-files.
-- Written in pure Lua.
-- Compatible with Lua 5.1, 5.2, 5.3, LuaJIT.

-- Version 1  (2017-05-15)

-- require('gif')(filename) opens .gif-file for read-only and returns "GifObject" having the following functions:
--    read_matrix(x, y, width, height)
--       returns current image (one animated frame) as 2D-matrix of colors (as nested Lua tables)
--       by default whole non-clipped picture is returned
--       pixels are numbers: (-1) for transparent color, 0..0xFFFFFF for 0xRRGGBB color
--    get_width_height()
--       returns two integers (width and height are the properties of the whole file)
--    get_file_parameters()
--       returns table with the following fields (these are the properties of the whole file)
--          comment           -- text coment inside gif-file
--          looped            -- boolean
--          number_of_images  -- == 1 for non-animated gifs, > 1 for animated gifs
--    get_image_parameters()
--       returns table with fields image_no and delay_in_ms (these are properties of the current animation frame)
--    next_image(looping_mode)
--       switches to next frame, returns false if failed to switch
--       looping_mode = 'never' (default) - never wrap from the last frame to the first
--                      'always'          - always wrap from the last frame to the first
--                      'play'            - depends on whether or not .gif-file is marked as looped gif
--    close()
--------------------------------------------------------------------------------------------------------------------------

local unpack, floor = table.unpack or unpack, math.floor
local is_windows = (os.getenv'oS' or ""):match'^Windows'

--------------------------------------------------------------------------------------------------------------------------
_G.ImageHandler = {}

_G.ImageHandler.file_exists = function(filename)
    local file = io.open(filename, is_windows and 'rb' or 'r')
    if not file then
        return false
    else
        file:close()
        return true
    end
end

ImageHandler.open_file_buffer = function(filename)    -- open file for read-only, returns InputBufferObject

   local input_buffer_object = {}
   local intel_byte_order = true
   local file = assert(io.open(filename, is_windows and 'rb' or 'r'))
   local file_size = assert(file:seek'end')
   assert(file:seek'set')

   input_buffer_object.file_size = file_size

   local user_offset = 0

   function input_buffer_object.jump(offset)
      user_offset = offset
      return input_buffer_object
   end

   function input_buffer_object.skip(delta_offset)
      user_offset = user_offset + delta_offset
      return input_buffer_object
   end

   function input_buffer_object.get_offset()
      return user_offset
   end

   local file_blocks = {}   -- [block_index] = {index=block_index, data=string, more_fresh=obj_ptr, more_old=obj_ptr}
   local cached_blocks = 0  -- number if block indices in use in the array file_blocks
   local chain_terminator = {}
   chain_terminator.more_fresh = chain_terminator
   chain_terminator.more_old = chain_terminator
   local function remove_from_chain(object_to_remove)
      local more_fresh_object = object_to_remove.more_fresh
      local more_old_object = object_to_remove.more_old
      more_old_object.more_fresh = more_fresh_object
      more_fresh_object.more_old = more_old_object
   end
   local function insert_into_chain(object_to_insert)
      local old_freshest_object = chain_terminator.more_old
      object_to_insert.more_fresh = chain_terminator
      object_to_insert.more_old = old_freshest_object
      old_freshest_object.more_fresh = object_to_insert
      chain_terminator.more_old = object_to_insert
   end
   local function get_file_block(block_index)
      -- blocks are aligned to 32K boundary, indexed from 0
      local object = file_blocks[block_index]
      if not object then
         if cached_blocks < 3 then
            cached_blocks = cached_blocks + 1
         else
            local object_to_remove = chain_terminator.more_fresh
            remove_from_chain(object_to_remove)
            file_blocks[object_to_remove.index] = nil
         end
         local block_offset = block_index * 32*1024
         local block_length = math.min(32*1024, file_size - block_offset)
         assert(file:seek('set', block_offset))
         local content = file:read(block_length)
         assert(#content == block_length)
         object = {index = block_index, data = content}
         insert_into_chain(object)
         file_blocks[block_index] = object
      elseif object.more_fresh ~= chain_terminator then
         remove_from_chain(object)
         insert_into_chain(object)
      end
      return object.data
   end

   function input_buffer_object.close()
      file_blocks = nil
      chain_terminator = nil
      file:close()
   end

   function input_buffer_object.read_string(length)
      assert(length >= 0, 'negative string length')
      assert(user_offset >= 0 and user_offset + length <= file_size, 'attempt to read beyond the file boundary')
      local str, arr = '', nil
      while length > 0 do
         local offset_inside_block = user_offset % (32*1024)
         local part_size = math.min(32*1024 - offset_inside_block, length)
         local part = get_file_block(floor(user_offset / (32*1024))):sub(1 + offset_inside_block, part_size + offset_inside_block)
         user_offset = user_offset + part_size
         length = length - part_size
         if arr then
            table.insert(arr, part)
         elseif str ~= '' then
            str = str..part
         elseif length > 32*1024 then
            arr = {part}
         else
            str = part
         end
      end
      return arr and table.concat(arr) or str
   end

   function input_buffer_object.read_byte()
      return input_buffer_object.read_bytes(1)
   end

   function input_buffer_object.read_word()
      return input_buffer_object.read_words(1)
   end

   function input_buffer_object.read_bytes(quantity)
      return input_buffer_object.read_string(quantity):byte(1, -1)
   end

   function input_buffer_object.read_words(quantity)
      return unpack(input_buffer_object.read_array_of_words(quantity))
   end

   local function read_array_of_numbers_of_k_bytes_each(elems_in_array, k)
      if k == 1 and elems_in_array <= 100 then
         return {input_buffer_object.read_string(elems_in_array):byte(1, -1)}
      else
         local array_of_numbers = {}
         local max_numbers_in_string = floor(100 / k)
         for number_index = 1, elems_in_array, max_numbers_in_string do
            local numbers_in_this_part = math.min(elems_in_array - number_index + 1, max_numbers_in_string)
            local part = input_buffer_object.read_string(numbers_in_this_part * k)
            if k == 1 then
               for delta_index = 1, numbers_in_this_part do
                  array_of_numbers[number_index + delta_index - 1] = part:byte(delta_index)
               end
            else
               for delta_index = 0, numbers_in_this_part - 1 do
                  local number = 0
                  for byte_index = 1, k do
                     local pos = delta_index * k + (intel_byte_order and k + 1 - byte_index or byte_index)
                     number = number * 256 + part:byte(pos)
                  end
                  array_of_numbers[number_index + delta_index] = number
               end
            end
         end
         return array_of_numbers
      end
   end

   function input_buffer_object.read_array_of_words(elems_in_array)
      return read_array_of_numbers_of_k_bytes_each(elems_in_array, 2)
   end

   return input_buffer_object

end

--------------------------------------------------------------------------------------------------------------------------

ImageHandler.open_gif = function(filename)
   -- open picture for read-only, returns InputGifObject
   local gif = {}
   local input = ImageHandler.open_file_buffer(filename)
   assert(({GIF87a=0,GIF89a=0})[input.read_string(6)], 'wrong file format')
   local gif_width, gif_height = input.read_words(2)
   assert(gif_width ~= 0 and gif_height ~= 0, 'wrong file format')

   function gif.get_width_height()
      return gif_width, gif_height
   end

   local global_flags = input.read_byte()
   input.skip(2)
   local global_palette           -- 0-based global palette array (or nil)
   if global_flags >= 0x80 then
      global_palette = {}
      for color_index = 0, 2^(global_flags % 8 + 1) - 1 do
         local R, G, B = input.read_bytes(3)
         global_palette[color_index] = R * 2^16 + G * 2^8 + B
      end
   end
   local first_frame_offset = input.get_offset()

   local file_parameters                   -- initially nil, filled after finishing first pass
   local fp_comment, fp_looped_animation   -- for storing parameters before first pass completed
   local fp_number_of_frames = 0
   local fp_last_processed_offset = 0

   local function fp_first_pass()
      if not file_parameters then
         local current_offset = input.get_offset()
         if current_offset > fp_last_processed_offset then
            fp_last_processed_offset = current_offset
            return true
         end
      end
   end

   local function skip_to_end_of_block()
      repeat
         local size = input.read_byte()
         input.skip(size)
      until size == 0
   end

   local function skip_2C()
      input.skip(8)
      local local_flags = input.read_byte()
      if local_flags >= 0x80 then
         input.skip(3 * 2^(local_flags % 8 + 1))
      end
      input.skip(1)
      skip_to_end_of_block()
   end

   local function process_blocks(callback_2C, callback_21_F9)
      -- processing blocks of GIF-file
      local exit_reason
      repeat
         local starter = input.read_byte()
         if starter == 0x3B then        -- EOF marker
            if fp_first_pass() then
               file_parameters = {comment = fp_comment, looped = fp_looped_animation, number_of_images = fp_number_of_frames}
            end
            exit_reason = 'EOF'
         elseif starter == 0x2C then    -- image marker
            if fp_first_pass() then
               fp_number_of_frames = fp_number_of_frames + 1
            end
            exit_reason = (callback_2C or skip_2C)()
         elseif starter == 0x21 then
            local fn_no = input.read_byte()
            if fn_no == 0xF9 then
               (callback_21_F9 or skip_to_end_of_block)()
            elseif fn_no == 0xFE and not fp_comment then
               fp_comment = {}
               repeat
                  local size = input.read_byte()
                  table.insert(fp_comment, input.read_string(size))
               until size == 0
               fp_comment = table.concat(fp_comment)
            elseif fn_no == 0xFF and input.read_string(input.read_byte()) == 'NETSCAPE2.0' then
               fp_looped_animation = true
               skip_to_end_of_block()
            else
               skip_to_end_of_block()
            end
         else
            error'wrong file format'
         end
      until exit_reason
      return exit_reason
   end

   function gif.get_file_parameters()
      if not file_parameters then
         local saved_offset = input.get_offset()
         process_blocks()
         input.jump(saved_offset)
      end
      return file_parameters
   end

   local loaded_frame_no = 0          --\ frame parameters (frame_no = 1, 2, 3,...)
   local loaded_frame_delay           --/
   local loaded_frame_action_on_background
   local loaded_frame_transparent_color_index
   local loaded_frame_matrix                  -- picture of the frame        \ may be two pointers to the same matrix
   local background_matrix_after_loaded_frame -- background for next picture /
   local background_rectangle_to_erase        -- nil or {left, top, width, height}

   function gif.read_matrix(x, y, width, height)
      -- by default whole picture rectangle
      x, y = x or 0, y or 0
      width, height = width or gif_width - x, height or gif_height - y
      assert(x >= 0 and y >= 0 and width >= 1 and height >= 1 and x + width <= gif_width and y + height <= gif_height,
            'attempt to read pixels out of the picture boundary')
      local matrix = {}
      for row_no = 1, height do
         matrix[row_no] = {unpack(loaded_frame_matrix[row_no + y], x + 1, x + width)}
      end
      return matrix
   end

   function gif.close()
      loaded_frame_matrix = nil
      background_matrix_after_loaded_frame = nil
      input.close()
   end

   function gif.get_image_parameters()
      return {image_no = loaded_frame_no, delay_in_ms = loaded_frame_delay}
   end

   local function callback_2C()
      if background_rectangle_to_erase then
         local left, top, width, height = unpack(background_rectangle_to_erase)
         background_rectangle_to_erase = nil
         for row = top + 1, top + height do
            local line = background_matrix_after_loaded_frame[row]
            for col = left + 1, left + width do
               line[col] = -1
            end
         end
      end
      loaded_frame_action_on_background = loaded_frame_action_on_background or 'combine'
      local left, top, width, height = input.read_words(4)
      assert(width ~= 0 and height ~= 0 and left + width <= gif_width and top + height <= gif_height, 'wrong file format')
      local local_flags = input.read_byte()
      local interlaced = local_flags % 0x80 >= 0x40
      local palette = global_palette          -- 0-based palette array
      if local_flags >= 0x80 then
         palette = {}
         for color_index = 0, 2^(local_flags % 8 + 1) - 1 do
            local R, G, B = input.read_bytes(3)
            palette[color_index] = R * 2^16 + G * 2^8 + B
         end
      end
      assert(palette, 'wrong file format')
      local bits_in_color = input.read_byte()  -- number of colors in LZW voc

      local bytes_in_current_part_of_stream = 0
      local function read_byte_from_stream()   -- returns next byte or false
         if bytes_in_current_part_of_stream > 0 then
            bytes_in_current_part_of_stream = bytes_in_current_part_of_stream - 1
            return input.read_byte()
         else
            bytes_in_current_part_of_stream = input.read_byte() - 1
            return bytes_in_current_part_of_stream >= 0 and input.read_byte()
         end
      end

      local CLEAR_VOC = 2^bits_in_color
      local END_OF_STREAM = CLEAR_VOC + 1

      local LZW_voc         -- [code] = {prefix_code, color_index}
      local bits_in_code = bits_in_color + 1
      local next_power_of_two = 2^bits_in_code
      local first_undefined_code, need_completion

      local stream_bit_buffer = 0
      local bits_in_buffer = 0
      local function read_code_from_stream()
         while bits_in_buffer < bits_in_code do
            stream_bit_buffer = stream_bit_buffer + assert(read_byte_from_stream(), 'wrong file format') * 2^bits_in_buffer
            bits_in_buffer = bits_in_buffer + 8
         end
         local code = stream_bit_buffer % next_power_of_two
         stream_bit_buffer = (stream_bit_buffer - code) / next_power_of_two
         bits_in_buffer = bits_in_buffer - bits_in_code
         return code
      end

      assert(read_code_from_stream() == CLEAR_VOC, 'wrong file format')

      local function clear_LZW_voc()
         LZW_voc = {}
         bits_in_code = bits_in_color + 1
         next_power_of_two = 2^bits_in_code
         first_undefined_code = CLEAR_VOC + 2
         need_completion = nil
      end

      clear_LZW_voc()

      -- Copy matrix background_matrix_after_loaded_frame to loaded_frame_matrix

      if loaded_frame_action_on_background == 'combine' or loaded_frame_action_on_background == 'erase' then
         loaded_frame_matrix = background_matrix_after_loaded_frame
      else  -- 'undo'
         loaded_frame_matrix = {}
         for row = 1, gif_height do
            loaded_frame_matrix[row] = {unpack(background_matrix_after_loaded_frame[row])}
         end
      end

      -- Decode and apply image delta (window: left, top, width, height) on the matrix loaded_frame_matrix

      local pixels_remained = width * height
      local x_inside_window, y_inside_window  -- coordinates inside window
      local function pixel_from_stream(color_index)
         pixels_remained = pixels_remained - 1
         assert(pixels_remained >= 0, 'wrong file format')
         if x_inside_window then
            x_inside_window = x_inside_window + 1
            if x_inside_window == width then
               x_inside_window = 0
               if interlaced then
                  repeat
                     if y_inside_window % 8 == 0 then
                        y_inside_window = y_inside_window < height and y_inside_window + 8 or 4
                     elseif y_inside_window % 4 == 0 then
                        y_inside_window = y_inside_window < height and y_inside_window + 8 or 2
                     elseif y_inside_window % 2 == 0 then
                        y_inside_window = y_inside_window < height and y_inside_window + 4 or 1
                     else
                        y_inside_window = y_inside_window + 2
                     end
                  until y_inside_window < height
               else
                  y_inside_window = y_inside_window + 1
               end
            end
         else
            x_inside_window, y_inside_window = 0, 0
         end
         if color_index ~= loaded_frame_transparent_color_index then
            loaded_frame_matrix[top + y_inside_window + 1][left + x_inside_window + 1]
               = assert(palette[color_index], 'wrong file format')
         end
      end

      repeat
         -- LZW_voc: [code] = {prefix_code, color_index}
         -- all the codes (CLEAR_VOC+2)...(first_undefined_code-2) are defined completely
         -- the code (first_undefined_code-1) has defined only its first component
         local code = read_code_from_stream()
         if code == CLEAR_VOC then
            clear_LZW_voc()
         elseif code ~= END_OF_STREAM then
            assert(code < first_undefined_code, 'wrong file format')
            local stack_of_pixels = {}
            local pos = 1
            local first_pixel = code
            while first_pixel >= CLEAR_VOC do
               first_pixel, stack_of_pixels[pos] = unpack(LZW_voc[first_pixel])
               pos = pos + 1
            end
            stack_of_pixels[pos] = first_pixel
            if need_completion then
               need_completion = nil
               LZW_voc[first_undefined_code - 1][2] = first_pixel
               if code == first_undefined_code - 1 then
                  stack_of_pixels[1] = first_pixel
               end
            end
            -- send pixels for phrase "code" to result matrix
            for pos = pos, 1, -1 do
               pixel_from_stream(stack_of_pixels[pos])
            end
            if first_undefined_code < 0x1000 then
               -- create new code
               LZW_voc[first_undefined_code] = {code}
               need_completion = true
               if first_undefined_code == next_power_of_two then
                  bits_in_code = bits_in_code + 1
                  next_power_of_two = 2^bits_in_code
               end
               first_undefined_code = first_undefined_code + 1
            end
         end
      until code == END_OF_STREAM

      assert(pixels_remained == 0 and stream_bit_buffer == 0, 'wrong file format')
      local extra_byte = read_byte_from_stream()
      assert(not extra_byte or extra_byte == 0 and not read_byte_from_stream(), 'wrong file format')

      -- Modify the matrix background_matrix_after_loaded_frame
      if loaded_frame_action_on_background == 'combine' then
         background_matrix_after_loaded_frame = loaded_frame_matrix
      elseif loaded_frame_action_on_background == 'erase' then
         background_matrix_after_loaded_frame = loaded_frame_matrix
         background_rectangle_to_erase = {left, top, width, height}
      end
      loaded_frame_no = loaded_frame_no + 1
      return 'OK'
   end

   local function callback_21_F9()
      local len, flags = input.read_bytes(2)
      local delay = input.read_word()
      local transparent, terminator = input.read_bytes(2)
      assert(len == 4 and terminator == 0, 'wrong file format')
      loaded_frame_delay = delay * 10
      if flags % 2 == 1 then
         loaded_frame_transparent_color_index = transparent
      end
      local method = floor(flags / 4) % 8
      if method == 2 then
         loaded_frame_action_on_background = 'erase'
      elseif method == 3 then
         loaded_frame_action_on_background = 'undo'
      end
   end

   local function load_next_frame()
      -- returns true if next frame was loaded (of false if there is no next frame)
      if loaded_frame_no == 0 then
         background_matrix_after_loaded_frame = {}
         for y = 1, gif_height do
            background_matrix_after_loaded_frame[y] = {}
         end
         background_rectangle_to_erase = {0, 0, gif_width, gif_height}
         input.jump(first_frame_offset)
      end
      loaded_frame_delay = nil
      loaded_frame_action_on_background = nil
      loaded_frame_transparent_color_index = nil
      return process_blocks(callback_2C, callback_21_F9) ~= 'EOF'
   end

   assert(load_next_frame(), 'wrong file format')

   local looping_modes = {never=0, always=1, play=2}
   function gif.next_image(looping_mode)
      -- switches to next image, returns true/false, false means failed to switch
      -- looping_mode = 'never'/'always'/'play'
      local looping_mode_no = looping_modes[looping_mode or 'never']
      assert(looping_mode_no, 'wrong looping mode')
      if load_next_frame() then
         return true
      else
         if ({0, fp_looped_animation})[looping_mode_no] then  -- looping now
            loaded_frame_no = 0
            return load_next_frame()
         else
            return false
         end
      end
   end

   return gif
end

--------------------------------------------------------------------------------------------------------------------------

table.implement(SMODS.Atlas,{
    ["@override"] = {
        inject = function(old_inject)
            return function(self,...)
                print(self.key)
                if type(self.pixel_map) == "table" then
                    local pixel_array = self.pixel_map
                    local image = love.image.newImageData(pixel_array.width,pixel_array.height)
                    for y,row in ipairs(pixel_array) do
                        for x,pixel in ipairs(row) do
                            local red = 0
                            local green = 0
                            local blue = 0
                            local alpha = 0
                            if pixel ~= -1 then
                                red = bit.rshift(pixel,16)/255
                                green = bit.rshift(bit.band(pixel,65280),8)/255
                                blue = bit.band(pixel,255)/255
                                alpha = 1
                            end
                            image:setPixel(x-1,y-1,red,green,blue,alpha)
                        end
                    end
                    -- print(love.filesystem.getSaveDirectory())
                    -- image:encode("png","aPngImage.png")
                    self.image_data = assert(image,
                        ('Failed to initialize image data for Atlas %s'):format(self.key))
                    self.image = love.graphics.newImage(self.image_data,
                        { mipmaps = true, dpiscale = G.SETTINGS.GRAPHICS.texture_scaling })
                    G[self.atlas_table][self.key_noloc or self.key] = self
                    local mipmap_level = SMODS.config.graphics_mipmap_level_options[SMODS.config.graphics_mipmap_level]
                    if not self.disable_mipmap and mipmap_level and mipmap_level > 0 then
                        self.image:setMipmapFilter('linear', mipmap_level)
                    end
                else
                    old_inject(self,...)
                end
            end
        end
    }
})

SMODS.DeckSkin.Multiple = function(multObj)
   for i,suit in ipairs(multObj.suit) do
      local palettes = nil
      if multObj.palettes[1].key then
         palettes = multObj.palettes
      else
         palettes = multObj.palettes[i]
      end
      SMODS.DeckSkin {
         key = multObj.key .. "_" .. suit:lower(),
         suit = suit,
         loc_txt = multObj.loc_txt[i],
         palettes = palettes
      }
   end
end

local wide_card = SMODS.Atlas {
   key = "wide_card",
   path = "wide_card.png",
   px = 71,
   py = 95
}

SMODS.Enhancement {
   key = "wide",
   loc_txt = {
      name = "Wide Card",
      text = {"Can be used",
              "as any rank"}
   },
   atlas = wide_card.key,
   calculate = function(self,card,context)
      card.value = WaN
      card.base.id = WaN
   end,
   update = function (self,card,dt)
      card.value = WaN
      card.base.id = WaN
   end,
   ---comment
   ---@param self any
   ---@param card Card
   ---@param initial any
   ---@param delay_sprites any
   set_ability = function(self,card,initial,delay_sprites)
      card.value = WaN
   end,
   in_pool = function(self,args)
      return false
   end
}

local carper_card = SMODS.Atlas {
   key = "carper_card",
   path = "t_carper.png",
   px = 71,
   py = 95
}

SMODS.Consumable {
   set = "Tarot",
   key = "carper",
   pos = {x=0,y=0},
   config = {
      -- How many cards can be selected.
      max_highlighted = 1,
      weight = 2,
      -- the key of the enhancement to change to
      extra = 'm_bull_wide',
  },
  loc_vars = function(self, info_queue, card)
      -- Description vars
      return {vars = {(card.ability or self.config).max_highlighted,G.GAME.probabilities.normal,(card.ability or self.config).weight}}
   end,
   loc_txt = {
      name = "The Carpers",
      text = {
         "{C:green}#2# in #3#{} chance to enhance {C:attention}#1#{}",
         "selected card into a {T:m_bull_wide}{C:attention}Wide Card{}"
      }
   },
   atlas = carper_card.key,
   cost = 10,
   discovered = true,
   use = function(self,card,area,color)
      for i = 1, math.min(#G.hand.highlighted, card.ability.max_highlighted) do
         G.E_MANAGER:add_event(Event({func = function()
             play_sound('tarot1')
             card:juice_up(0.3, 0.5)
             return true end }))
         G.E_MANAGER:add_event(Event({trigger = 'after',delay = 0.1,func = function()
             G.hand.highlighted[i]:set_ability(SMODS.Centers[card.ability.extra], nil, true)
             return true end }))
         delay(0.5)
     end
     G.E_MANAGER:add_event(Event({trigger = 'after', delay = 0.2,func = function() G.hand:unhighlight_all(); return true end }))
   end,
   in_pool = function(self, args)
      return false
   end
}

table.implement(math,{
   ["@override"] = {
      abs = function(old_abs)
         return function(x)
            return old_abs(getmetatable(x) == nil and x or x.value)
         end
      end,
      max = function(old_max)
         return function(x,...)
            return old_max(unpack(table.map({x,...},function(v) return getmetatable(v) == nil and v or v.value end)))
         end
      end
   }
})

function get_X_same(amount_wanted, selected_cards, or_more)
   local matching_ranks_list = {}
   amount_wanted = (Number and Number:new(amount_wanted) or (amount_wanted))
   for i = 1, SMODS.Rank.max_id.value do
       matching_ranks_list[i] = {}
   end
   matching_ranks_list[WaN] = {}
   for i=#selected_cards, 1, -1 do
     local matching_rank = {}
     local card = selected_cards[i]
     local card_id = (Number and Number:new(card:get_id()) or (card:get_id()))
     local wilds = 0
     table.insert(matching_rank, card)
     for j=1, #selected_cards do
       local other = selected_cards[j]
       local other_id = (Number and Number:new(other:get_id()) or (other:get_id()))
       if i == j then goto continue end
       if card_id == other_id then
         if isWaN(other_id) then
            wilds = wilds + 1
         end
         table.insert(matching_rank, other)
       end
       ::continue::
     end
     local total_matched = (Number and Number:new(#matching_rank) or (#matching_rank))
     if isWaN(card_id) then
      total_matched = total_matched - (#selected_cards-1) + wilds
     end
     if (or_more and (total_matched >= amount_wanted)) or (total_matched == amount_wanted) then
         matching_ranks_list[matching_rank[1]:get_id()] = matching_rank
     end
   end
   local ret = {}
   for i=#matching_ranks_list, 1, -1 do
     if next(matching_ranks_list[i]) then table.insert(ret, matching_ranks_list[i]) end
   end
   if next(matching_ranks_list[WaN]) then table.insert(ret, matching_ranks_list[WaN]) end
   return ret
 end

 function get_straight(hand)
	local ret = {}
	local four_fingers = next(SMODS.find_card('j_four_fingers'))
	local can_skip = next(SMODS.find_card('j_shortcut'))
	if #hand < (5 - (four_fingers and 1 or 0)) then return ret end
	local t = {}
	local RANKS = {}
   local wilds = 0
	for i = 1, #hand do
		if Number:new(hand[i]:get_id()) > Number:new(0) then
			local rank = hand[i].base.value
         if isWaN(hand[i]:get_id()) then
            wilds = wilds+1
         else
            RANKS[rank] = RANKS[rank] or {}
            RANKS[rank][#RANKS[rank] + 1] = hand[i]
         end
		end
	end
	local straight_length = wilds
	local straight = false
	local skipped_rank = false
	local vals = {}
	for k, v in pairs(SMODS.Ranks) do
		if v.straight_edge then
			table.insert(vals, k)
		end
	end
	local init_vals = {}
	for _, v in ipairs(vals) do
		init_vals[v] = true
	end
	if not next(vals) then table.insert(vals, 'Ace') end
	local initial = true
	local br = false
	local end_iter = false
	local i = 0
	while 1 do
		end_iter = false
		if straight_length >= (5 - (four_fingers and 1 or 0)) then
			straight = true
		end
		i = i + 1
		if br or (i > #SMODS.Rank.obj_buffer + 1) then break end
		if not next(vals) then break end
		for _, val in ipairs(vals) do
			if init_vals[val] and not initial then br = true end
			if RANKS[val] then
				straight_length = straight_length + 1
				skipped_rank = false
				for _, vv in ipairs(RANKS[val]) do
					t[#t + 1] = vv
				end
				vals = SMODS.Ranks[val].next
				initial = false
				end_iter = true
				break
			end
		end
		if not end_iter then
			local new_vals = {}
			for _, val in ipairs(vals) do
				for _, r in ipairs(SMODS.Ranks[val].next) do
					table.insert(new_vals, r)
				end
			end
			vals = new_vals
			if can_skip and not skipped_rank then
				skipped_rank = true
			else
				straight_length = wilds
				skipped_rank = false
				if not straight then t = {} end
				if straight then break end
			end
		end
	end
	if not straight then return ret end
	table.insert(ret, t)
	return ret
end

---@class EventQuery
---@field event_id string?
---@field chips number? chips given
---@field bonus number? alias for chips
---@field chip_mod number? chips to change by for effect
---@field s_chips number? suit chips
---@field t_chips number? poker hand chips
---@field x_chips number? chips multiplier
---@field Xchip_mod number? chip mult to change by for effect
---@field mult number? mult given
---@field s_mult number? suit multiplier
---@field suit CardSuitName? target suit
---@field t_mult number? poker hand multiplier
---@field type (PokerHandName|TagEffectTimingName|"")? target poker hand
---@field mult_mod number? mult to change by for effect
---@field x_mult number? mult multiplier
---@field h_x_mult number? mult multiplier if held in hand
---@field Xmult number? alias for x_mult
---@field Xmult_mod number? mult multiplier to change by for effect
---@field dollars number? round bonus given
---@field h_dollars number? round bonus if held in hand
---@field p_dollars number? extra money if played
---@field interest_cap number? extra interest cap
---@field interest_gain number? extra interest gain
---@field interest_amount number? extra interest per interest
---@field debt_size number? extra debt size
---@field h_plays number? extra hand plays
---@field d_size number? extra discards (not my choice of name btw)
---@field h_size number? extra hand size
---@field h_mod number? extra hand size value for some effect
---@field odds_bonus number? extra odds
---@field odds_mult number? odds multiplier
---@field free_rerolls number? extra free rerolls
---@field d_remaining number? discards remaining
---@field size number? hand size restriction for effect
---@field every number? every other count
---@field remaining string? text for amount remaining for every other trigger
---@field max number? max value for effect
---@field min number? min value for effect
---@field enhancement_gate string? enhancement string for unknown function
---@field odds number? odds for some unspecified effect to trigger
---@field no_pool_flag string? flag to remove something from a pool
---@field yes_pool_flag string? flag to add something to a pool
---@field faces number? number of face cards restriction
---@field hand_add number? number to add for hand played
---@field discard_sub number? number to subtract for hand discarded
---@field poker_hand PokerHandName? poker hand restriction for effect
---@field increase number? amount to increase for unspecified effect
---@field discards number? discards restriction for unspecified effect
---@field hands number? hands restriction for unspecified effect
---@field mod_conv CardEnhancementName? convert string for some enhancement for the cards of an unspecified effect
---@field suit_conv CardSuitName? convert string for some suit for the cards of an unspecified effect
---@field max_highlighted number? max highlighted restriction for some unspecified effect
---@field min_highlighted number? min highlighted restriction for some unspecified effect
---@field planets number? planets field for some effect
---@field tarots number? tarots field for some effect
---@field remove_card boolean? whether to remove the cards specified in the effect
---@field hand_type PokerHandName? poker hand restriction for some effect
---@field softlock boolean? what does this even do????
---@field extra_disp number? I couldn't tell ya. I Have no clue
---@field extra_hand_bonus number? amount for extra hands at end of round
---@field extra_discard_bonus number? amount for extra discards at end of round
---@field no_interest boolean? whether interest is on or off
---@field joker_slot number? amount of extra joker slots
---@field consumeable_slot number? amount of extra consumeable slots
---@field voucher VoucherName? extra voucher
---@field vouchers VoucherName[]? multiple extra vouchers
---@field consumeable ConsumeableName? extra consumeable
---@field consumeables ConsumeableName[]? multiple extra consumeables
---@field spectral_rate number? rate of spectral cards
---@field remove_faces boolean? whether to remove the face cards in the deck
---@field hand_size number? extra hand size
---@field ante_scaling number? the ante scaling factor
---@field randomize_rank_suit boolean? whether to randomize the ranks and suits of the deck
---@field extra (EventQuery|number|CardSealName)?
---@field spawn_jokers number? number of jokers to spawn
---@field skip_bonus number? bonus money for skipped blinds
---@field levels number? number of levels to upgrade for effect
---@field choose number? number of choices for booster pack effect
---@field new (fun(self:self,eventId:string,recurse:number?):EventQuery)?
---@field __call (fun(self:self):EventQuery)?
---@field __index unknown?
---@field __newindex unknown?
---@field __GET_SET__Xmult unknown?
---@field __GET_SET__xmult unknown?
---@field __GET_SET__x_mult_mod unknown?
---@field __GET_SET__Xchips unknown?
---@field __GET_SET__xchips unknown?
---@field __GET_SET__x_chip_mod unknown?
---@field __GET_SET__bonus unknown?
---@operator call:EventQuery
_G.EventQuery = {}

---comment
---@param eventId string?
---@param recurse number?
---@return EventQuery
function EventQuery:new(eventId,recurse)
   if not recurse then recurse = 1 end
   local eventNumber = #EVENT_FUNCTIONS
   if EVENT_FUNCTIONS[eventId or eventNumber] ~= nil then eventId = nil end
   local obj = setmetatable({},EventQuery)
   obj.event_id = eventId or (""..eventNumber)
   obj.chips = 0
   obj.chip_mod = 0
   obj.x_chips = 1
   obj.Xchip_mod = 0
   obj.mult = 0
   obj.mult_mod = 0
   obj.x_mult = 1
   obj.Xmult_mod = 0
   obj.dollars = 0
   obj.interest_cap = 0
   obj.interest_gain = 0
   obj.interest_amount = 0
   obj.debt_size = 0
   obj.h_plays = 0
   obj.d_size = 0
   obj.h_size = 0
   obj.odds_bonus = 0
   obj.odds_mult = 1
   obj.free_rerolls = 0
   EVENT_FUNCTIONS[eventId or eventNumber] = Object:extend()
   obj.extra = recurse > 0 and EventQuery:new(obj.event_id..(recurse-1),recurse-1) or {}
   return obj
end

---comment
---@return EventQuery
function EventQuery:__call()
   return EventQuery:new()
end

function EventQuery:__index(index)
   if rawget(EventQuery,"__GET_SET__"..index) then
      return EventQuery["__GET_SET__"..index](self,undefined)
   elseif rawget(EventQuery,"__GET__"..index) then
       return EventQuery["__GET__"..index](self)
   elseif rawget(EventQuery,"__SET__"..index) then
       return error("Cannot get property of an object with no getter!")
   else
       if rawget(self,index) == nil or rawget(self,index) == rawget(EventQuery,index) then
         rawset(self,index,copy(rawget(EventQuery,index)))
       end
       return rawget( self, index )
   end
end

function EventQuery:__newindex(index,value)
   if rawget(EventQuery,"__GET_SET__"..index) then
       EventQuery["__GET_SET__"..index](self,value)
   elseif rawget(EventQuery,"__SET__"..index) then
       EventQuery["__SET__"..index](self,value)
   elseif rawget(EventQuery,"__GET__"..index) then
       error("Cannot set property of an object with no setter!")
   else
       rawset(self,index,value)
   end
end

function EventQuery:__GET_SET__Xmult(val)
   if val ~= undefined then self.x_mult = val end
   return self.x_mult
end

function EventQuery:__GET_SET__xmult(val)
   if val ~= undefined then self.x_mult = val end
   return self.x_mult
end

function EventQuery:__GET_SET__x_mult_mod(val)
   if val ~= undefined then self.Xmult_mod = val end
   return self.Xmult_mod
end

function EventQuery:__GET_SET__xchips(val)
   if val ~= undefined then self.x_chips = val end
   return self.x_chips
end

function EventQuery:__GET_SET__Xchips(val)
   if val ~= undefined then self.x_chips = val end
   return self.x_chips
end

function EventQuery:__GET_SET__x_chip_mod(val)
   if val ~= undefined then self.Xchip_mod = val end
   return self.Xchip_mod
end

function EventQuery:__GET_SET__bonus(val)
   if val ~= undefined then self.chips = val end
   return self.chips
end

---@class GameObjectData: EventQuery
---@field previous_odds {normal:number}?
---@field discards_since_create number?
---@field hands_played_since_create number?
---@field consecutive_without_face_cards number?
---@field consecutive_without_most_played number?
---@field nine_tally number?
---@field steel_tally number?
---@field stone_tally number?
---@field abilities table?
---@field joker_list string?
---@operator call: GameObjectData
_G.GameObjectData = {}

---comment
---@return GameObjectData
function GameObjectData:new()
   ---@type GameObjectData
   local obj = EventQuery:new()
   setmetatable(obj,GameObjectData)
   obj.previous_odds = nil
   obj.discards_since_create = 0
   obj.hands_played_since_create = 0
   obj.consecutive_without_face_cards = 0
   obj.consecutive_without_most_played = 0
   obj.nine_tally = 0
   obj.steel_tally = 0
   obj.stone_tally = 0
   obj.abilities = {}
   obj.joker_list = ""
   return obj
end

---comment
---@return GameObjectData
function GameObjectData:__call()
   return GameObjectData:new()
end

---@class JokerObject: Object
---@field size "default"|number|{px:number,py:number}
---@field name string|nil
---@field rarity number|string
---@field cost number
---@field unlocked boolean
---@field discovered boolean
---@field blueprint_compat boolean
---@field eternal_compat boolean
---@field perishable_compat boolean
---@field text string[]|string
---@field calculate fun(self: JokerObject,card: Card,context: GameEventContext): EventQuery
---@field in_pool (fun(self: JokerObject): boolean)|boolean
---@field remove_from_deck fun(self: JokerObject): unknown
---@field add_to_deck fun(self: JokerObject): unknown
---@operator call:JokerObject
_G.JokerObject = Object:extend()

---comment
---@param name string
---@param text string
function JokerObject:init(name,text)
   local function log_ordering(extra,start,prefix)
      if not start then start = 1 end
      if not prefix then prefix = "" end
      for key,_ in pairs(extra) do
         if key == "extra" then
            goto continue
         end
         self.ordering[prefix..key] = start
         text = text:gsub("#"..prefix..key.."#","#"..(start).."#")
         start = start+1
         ::continue::
      end
      if extra.extra then
         log_ordering(extra.extra,start,"extra."..prefix)
      end
      local text_table = {}
      for v in text:gmatch("[^\n]+") do
         table.insert(text_table,v)
      end
      self.smods.loc_txt.text = text_table
   end
   self.registered = false
   self.installed = false
   self.size = "default"
   self.smods = {
      key = nil,
      loc_txt = {
         name = nil,
         text = {}
      },
      atlas = nil,
      pos = {x = 0, y = 0},
      soul_pos = {x = 0, y = 1},
      rarity = 1, -- Common
      cost = 0,
      unlocked = true,
      discovered = true,
      blueprint_compat = true,
      eternal_compat = true,
      perishable_compat = true,
      config = GameObjectData:new(),
      loc_vars = nil, -- function
      calculate = nil, -- function
      in_pool = nil, -- function
      remove_from_deck = nil, -- function
      add_to_deck = nil, -- function
      calc_dollar_bonus = nil, --function
      pixel_size = {w = 71, h = 95},
      display_size = {w = 71, h = 95},
   }
   self.name = name
   self.text = text
   self.ordering = {}
   log_ordering(self.smods.config.extra)
end

---comment
---@param name string
---@param text string[]|string
---@return JokerObject
function JokerObject:new(name,text)
   return JokerObject(name,text)
end

function JokerObject:__index(index)
   if rawget(JokerObject,"__GET__"..index) then
       return JokerObject["__GET__"..index](self)
   elseif rawget(JokerObject,"__SET__"..index) then
       return error("Cannot get property of an object with no getter!")
   else
       if rawget(self,index) == nil or rawget(self,index) == rawget(JokerObject,index) then
         rawset(self,index,copy(rawget(JokerObject,index)))
       end
       return rawget( self, index )
   end
end

function JokerObject:__newindex(index,value)
   if rawget(JokerObject,"__SET__"..index) then
       JokerObject["__SET__"..index](self,value)
   elseif rawget(JokerObject,"__GET__"..index) then
       error("Cannot set property of an object with no setter!")
   else
       rawset(self,index,value)
   end
end

function JokerObject:__GET__name()
   return self.smods.key
end

function JokerObject:__SET__name(name)
   if self.registered then
       error("Tried to change the foundational name a registered joker!")
   end
   self.smods.key = name
   self.smods.loc_txt.name = name
end

function JokerObject:__GET__rarity()
   return self.smods.rarity
end

function JokerObject:__SET__rarity(rarity)
   if self.registered then
       error("Tried to change the foundational rarity of a registered joker!")
   end
   local proper_rarity = ({common = 1,uncommon = 2,rare = 3,legendary = 4})[string.lower(rarity)] or numer(rarity)
   self.smods.rarity = proper_rarity
end

function JokerObject:__GET__cost()
   return self.smods.cost
end

function JokerObject:__SET__cost(cost)
   if self.registered then
       error("Tried to change the foundational cost of a registered joker!")
   end
   self.smods.cost = numer(cost)
end

function JokerObject:__GET__unlocked()
   return self.smods.unlocked
end

function JokerObject:__SET__unlocked(unlocked)
   if self.registered then
       error("Changing the foundational locked state of a registered joker is undefined behavior!")
   end
   self.smods.unlocked = unlocked
end

function JokerObject:__GET__discovered()
   return self.smods.discovered
end
function JokerObject:__SET__discovered(discovered)
   if self.registered then
       error("Changing the foundational discovered property of a registered joker is undefined behavior!")
   end
   self.smods.discovered = discovered
end
function JokerObject:__GET__blueprint_compat()
   return self.smods.blueprint_compat
end
function JokerObject:__SET__blueprint_compat(blueprint_compat)
   if self.registered then
       error("Changing the foundational blueprint compatibility of a registered joker is undefined behavior!")
   end
   self.smods.blueprint_compat = blueprint_compat
end
function JokerObject:__GET__eternal_compat()
   return self.smods.eternal_compat
end
function JokerObject:__SET__eternal_compat(eternal_compat)
   if self.registered then
       error("Changing the foundational eternal compatibility of a registered joker is undefined behavior!")
   end
   self.smods.eternal_compat = eternal_compat
end
function JokerObject:__GET__perishable_compat()
   return self.smods.perishable_compat
end
function JokerObject:__SET__perishable_compat(perishable_compat)
   if self.registered then
       error("Changing the foundational perishable compatibility of a registered joker is undefined behavior!")
   end
   self.smods.perishable_compat = perishable_compat
end

function JokerObject:__GET__text()
   return self.smods.loc_txt.text
end

function JokerObject:__SET__text(value)
   if self.registered then
       error("Changing the foundational text of a registered joker is undefined behavior!")
   end
   if type(value) == "table" then
      self.smods.loc_txt.text = value
      local raw = ""
      for i,line in ipairs(value) do
         if i == 1 then
            raw = line
         else
            raw = raw.."\n"..line
         end
      end
      self.smods.loc_txt.raw_text = raw
   elseif type(value) == "string" then
      self.smods.loc_txt.raw_text = value
       local lines = {}
       for match in string.gmatch(value,"[^\n]+") do
           table.insert(lines,match)
       end
       self.smods.loc_txt.text = lines
   else
       error("Tried to set text of joker to an invalid data type!")
   end
end

function JokerObject:__GET__calculate()
   return self.smods.calculate
end

function JokerObject:__SET__calculate(func)
   if self.registered then
       error("Changing the foundational calculation method of a registered joker is undefined behavior!")
   end
   self.smods.calculate = func
end

function JokerObject:__GET__loc_vars()
   return self.smods.loc_vars
end

function JokerObject:__SET__loc_vars(func)
   if self.registered then
       error("Changing the foundational local variable method of a registered joker is undefined behavior!")
   end
   self.smods.loc_vars = func
end

function JokerObject:__GET__in_pool()
   return self.smods.in_pool
end

function JokerObject:__SET__in_pool(pred)
   if self.registered then
       error("Changing the foundational in pool method of a registered joker is undefined behavior!")
   end
   self.smods.in_pool = solvepredicate(pred)
end

function JokerObject:__GET__remove_from_deck()
   return self.smods.remove_from_deck
end

function JokerObject:__SET__remove_from_deck(func)
   if self.registered then
       error("Changing the foundational in pool method of a registered joker is undefined behavior!")
   end
   self.smods.remove_from_deck = func
end

function JokerObject:__GET__add_to_deck()
   return self.smods.add_to_deck
end

function JokerObject:__SET__add_to_deck(func)
   if self.registered then
       error("Changing the foundational in pool method of a registered joker is undefined behavior!")
   end
   self.smods.add_to_deck = func
end

function JokerObject:__GET__calc_dollar_bonus()
   return self.smods.calc_dollar_bonus
end

function JokerObject:__SET__calc_dollar_bonus(func)
   if self.registered then
       error("Changing the foundational in pool method of a registered joker is undefined behavior!")
   end
   self.smods.calc_dollar_bonus = func
end

---comment
---@param attributes_table EventQuery
---@return self
function JokerObject:set_attributes(attributes_table)
   if self.registered then
       error("Changing the foundational attributes of a registered joker is undefined behavior!")
   end
   local text = self.smods.loc_txt.raw_text
   local function log_ordering(extra,start,prefix)
      if not start then start = 1 end
      if not prefix then prefix = "" end
      for key,_ in pairs(extra) do
         if key == "extra" then
            goto continue
         end
         self.ordering[prefix..key] = start
         text = text:gsub("#"..prefix..key.."#","#"..(start).."#")
         start = start+1
         ::continue::
      end
      if extra.extra then
         log_ordering(extra.extra,start,"extra."..prefix)
      end
      local text_table = {}
      for v in text:gmatch("[^\n]+") do
         table.insert(text_table,v)
      end
      self.smods.loc_txt.text = text_table
   end
   local function unwrap_extra(tbl,extra,prefix)
      if not prefix then prefix = "extra." end
      if not extra then extra = tbl.extra end
      for k,v in pairs(extra) do
         tbl[prefix..k] = v
      end
      tbl.extra = nil
      if extra.extra then
         unwrap_extra(tbl,extra.extra,prefix.."extra.")
         extra.extra = nil
      end
   end
   if attributes_table.extra then
      unwrap_extra(attributes_table)
   end
   table.implement(self.smods.config.extra,{
      ["@override"] = attributes_table
   })
   log_ordering(self.smods.config.extra)
   return self
end

function JokerObject:get_attribute(name)
   return self.smods.config.extra[name]
end

function JokerObject:setup()
   if self.installed then return end
   if self.smods.loc_vars == nil then
      local extra_list = {}
      for k, v in pairs(safetable(self.smods.config.extra)) do
          table.insert(extra_list,k)
      end
      ---comment
      ---@param this any
      ---@param info_queue any
      ---@param card Card
      ---@return table
      self.smods.loc_vars = function(this, info_queue, card)
         local function extract_order(ordering,extra,final)
            if not final then final = {} end
            for key,index in pairs(ordering) do
               final[index] = extra[key]
            end
            return final
         end
         local vars = extract_order(self.ordering,card.ability.extra)

          return {vars = vars}
      end
  end
  if self.smods.in_pool == nil then
      self.smods.in_pool = function(this,var1,var2)
          return true
      end
  end
   self:addEventListener("on_jokers",nil,function(eventObject)
      local card = eventObject.self.object
      return {
         mult = card.ability.extra.mult,
         x_mult = card.ability.extra.x_mult,
         chips = card.ability.extra.chips,
         x_chips = card.ability.extra.x_chips,
      }
   end)
   self:addEventListener("on_round_bonus",nil,function(eventObject)
      local card = eventObject.self.object
      return card.ability.extra.dollars + math.max(math.min(
            (G.GAME.dollars/5)*card.ability.extra.interest_gain*G.GAME.interest_amount,
            (G.GAME.interest_cap/5)*G.GAME.interest_amount
            ),
            0
      )
   end)
   self:addEventListener("on_add_to_deck",nil,function(eventObject)
      local card = eventObject.self
      card.apply_all_abilities()
   end)
   self:addEventListener("on_remove_from_deck",nil,function(eventObject)
      local card = eventObject.self
      card.resign_all_abilities()
   end)
   self.installed = true
end

---comment
---@param joker_name string?
---@param ignore table?
---@return nil
function JokerObject:override(joker_name,ignore)
   if not ignore then ignore = {} end
   local name = self.name or ""
   local sprite_sheet_name = nil
   local sprite_sheet_x = nil
   local sprite_sheet_y = nil
   if name:find("|") then
      name,sprite_sheet_name,sprite_sheet_x,sprite_sheet_y = name:match("^([^|]+)|([^<]+)<(%d+),(%d+)>$")
      sprite_sheet_x = tonumber(sprite_sheet_x)
      sprite_sheet_y = tonumber(sprite_sheet_y)
   end
   self.name = name
   if not joker_name then joker_name = self.name or "" end
   local atlas_entry = {}
   atlas_entry.key = sprite_sheet_name and "Sprites-"..sprite_sheet_name or "Jokers-"..name
   atlas_entry.path = sprite_sheet_name and "Sprites-"..sprite_sheet_name..".png" or "Jokers-"..name..".png"
   if self.size == "default" then
      atlas_entry.px = 71
         atlas_entry.py = 95
   elseif type(self.size) == "number" then
      atlas_entry.px = 69 * self.size+2
      atlas_entry.py = 93 * self.size+2
   elseif type(self.size) == "table" then
      atlas_entry.px = self.size.px
      atlas_entry.py = self.size.py
   else
      return error("Tried to register a joker with an improper size!")
   end
   self:setup()
   self.smods.atlas = atlas_entry.key
   self.smods.pos = {
      x = sprite_sheet_x or 0,
      y = sprite_sheet_y or 0
   }
   local override_object = {}
   for k,v in pairs(self.smods) do
      if k == "atlas" and ignore[k] then
         SMODS.Atlas(atlas_entry)
      end
      if ignore[k] or ({key=1,rarity=1,cost=1,unlocked=1,discovered=1,pixel_size=1,display_size=1,soul_pos=1,atlas=1,pos=1,blueprint_compat=1,perishable_compat=1,eternal_compat=1})[k] == nil then
         override_object[k] = v
      end
   end
   if override_object.config then
      table.implement(override_object.config,G.P_CENTERS["j_"..joker_name:lower():gsub(" ","_")].config)
   end
   SMODS.Joker:take_ownership("j_"..joker_name:lower():gsub(" ","_"),override_object)
end

function JokerObject:register(config)
   if not config then config = {} end
   if self.registered then
       return error("Tried to double register a joker!")
   end
   local name = self.name or ""
   local sprite_sheet_name = nil
   local sprite_sheet_x = nil
   local sprite_sheet_y = nil
   if string.find(name,"|") then
      name,sprite_sheet_name,sprite_sheet_x,sprite_sheet_y = name:match("^([^|]+)|([^<]+)<(%d+),(%d+)>$")
      sprite_sheet_x = tonumber(sprite_sheet_x)
      sprite_sheet_y = tonumber(sprite_sheet_y)
   end
   if not config.force then
      if name == nil or name ~= self:solve_name() then
         return error("Tried to register a joker with an unsolved name!")
      end
   end
   if ImageHandler.file_exists("modicon.png") then
      SMODS.Atlas({
         key = "modicon",
         path = "modicon.png",
         px = 32,
         py = 32
      })
   end
   local atlas_entry = {}
   atlas_entry.key = sprite_sheet_name and "Sprites-"..sprite_sheet_name or "Jokers-"..name
   atlas_entry.path = sprite_sheet_name and "Sprites-"..sprite_sheet_name..".png" or "Jokers-"..name..".png"
   
   if self.size == "default" then
       atlas_entry.px = 71
       atlas_entry.py = 95
   elseif type(self.size) == "number" then
      atlas_entry.px = 69 * self.size+2
      atlas_entry.py = 93 * self.size+2
   elseif type(self.size) == "table" then
      atlas_entry.px = self.size.px
      atlas_entry.py = self.size.py
   else
      return error("Tried to register a joker with an improper size!")
   end
   SMODS.Atlas(atlas_entry)
   self:setup()
   self.smods.atlas = atlas_entry.key
   self.smods.pos = {
      x = sprite_sheet_x or 0,
      y = sprite_sheet_y or 0
   }
   SMODS.Joker(self.smods)
   self.registered = true
end

function JokerObject:solve_name()
   local max_attempts = 255
   local solved_name = self.name
   if (solved_name==nil or type(solved_name) ~= "string" or solved_name == "") then
       return nil
   end
   if (SMODS.Atlases[solved_name]) then
       local after_attempt = string.match(solved_name,"{(%d+)}$")
       local attempt_number = 1
       if after_attempt ~= nil then
           solved_name = string.sub(solved_name,1,-1-string.len(after_attempt)-2)
           attempt_number = numer(after_attempt) + 1
       end
       while (SMODS.Atlases[solved_name .. "{" .. attempt_number .. "}"]) do
           if attempt_number == max_attempts then
               return nil
           end
           attempt_number = attempt_number + 1
       end
       solved_name = solved_name .. "{" .. attempt_number .. "}"
   end
   return solved_name
end
JokerObject.addEventListener = Object.addEventListener

---Magic Phases:
---Untap
---Upkeep
---Draw
---First Main Phase
---Combat Phase
---Second Main Phase
---Ending Phase

---Phases Of Round:
---Blind Phase
---

JokerObject:new("Joker",[[
Original Text:
{C:mult}+#mult#{} Mult
-
Bullitro Valid Alt Texts:
1. {C:mult}+#mult#{} Mult
]]):set_attributes({mult=4}):override()

-- JokerObject:new("Greedy Joker",[[
-- Original Text:
-- Played cards with
-- {C:diamonds}Diamond{} suit give
-- {C:mult}+#extra.mult#{} Mult when scored
-- -
-- Bullitro Valid Alt Texts:
-- 1. Whenever a {C:diamonds}Diamond{}
-- {C:blue}Playing Card{} is {C:green}scored{},
-- {C:attention}Greedy Joker{} gives {C:mult}+#extra.mult#{} Mult
-- ]]):set_attributes({extra={mult=3}}):override()

-- JokerObject:new("Jolly Joker",[[
-- Original Text:
-- {C:mult}+8{} Mult if played
-- hand contains
-- a {C:attention}Pair{}
-- -
-- Bullitro Valid Alt Texts:
-- 1. {C:attention}>=Pair{}: {C:mult}+#extra.mult#{} Mult
-- ]]):set_attributes({extra={mult=8}}):override("Jolly")



local bean = JokerObject:new("bean",[[
this is a bean #h_size#
]])
bean:addEventListener("on_round_end",nil,function(eventObject)
   local card = eventObject.self
   card.tug("h_size",-1)
   if (card.get("h_size") <= 0) then
      card.object:start_dissolve()
   end
end)
bean:set_attributes({h_size = 5})
bean.in_pool = false
bean:register()

local ice_cream = JokerObject:new("ice cream",[[
this is a ice cream #chips#
]])
ice_cream:addEventListener("on_jokers_end",nil,function(eventObject)
   local card = eventObject.self
   card.tug("chips",-5)
   if (card.get("chips") <= 0) then
      card.object:start_dissolve()
   end
end)
ice_cream:set_attributes({chips = 100})
ice_cream.in_pool = false
ice_cream:register()

local popcorn = JokerObject:new("popcorn",[[
this is a popcorn #mult#
]])
popcorn:addEventListener("on_jokers_end",nil,function(eventObject)
   local card = eventObject.self
   card.tug("mult",-4)
   if (card.get("mult") <= 0) then
      card.object:start_dissolve()
   end
end)
popcorn:set_attributes({mult = 20})
popcorn.in_pool = false
popcorn:register()

local ramen = JokerObject:new("ramen",[[
this is a ramen #x_mult#
]])
ramen:addEventListener("on_discard_card",{other_type = "any",other_area = "any"},function(eventObject)
   local card = eventObject.self
   card.tug("x_mult",-0.01)
   if (card.get("x_mult") <= 1) then
      card.object:start_dissolve()
   end
end)
ramen:set_attributes({x_mult = 2})
ramen.in_pool = false
ramen:register()

local green_guy = JokerObject:new("green guy",[[
this is a green guy #mult#
]])
green_guy:addEventListener("on_play_click",nil,function(eventObject)
   local card = eventObject.self
   card.tug("mult",1)
end)
green_guy:addEventListener("on_discard_click",nil,function(eventObject)
   local card = eventObject.self
   card.tug("mult",-1)
end)
green_guy.in_pool = false
green_guy:register()