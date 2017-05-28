# Write a parallel reduce function along the lines of the parallel map function
# we just created.

defmodule Parallel do
  # From book:
  def map(collection, fun) do
    parent = self()  # who worker processes will send result

    processes = Enum.map(collection, fn(e) ->
        spawn_link(fn ->  # worker process
            # compute the function on the element and return the result
            send(parent, {self(), fun.(e)})
          end)
      end)

    Enum.map(processes, fn(pid) ->
        receive do
          # get the result from each worker
          {^pid, result} -> result
        end
      end)
  end

  # Written by me:
  def reduce(collection, fun) do
    # parallelize in log(n)

    len = length collection
    if len == 1 do
      # just one element, return it
      Enum.at collection, 0
    else
      parent = self()  # who worker processes will send result
      {left_half, right_half} = Enum.split collection, round(len/2)

      # assign work to the left and right processes
      processes = Enum.map [left_half, right_half], fn(half) ->
        spawn_link fn ->
          # recurse
          result = Parallel.reduce half, fun
          send parent, {self(), result}
        end
      end

      # collect results
      [left_result, right_result] = Enum.map processes, fn(process) ->
        receive do
          {^process, result} -> result
        end
      end

      # reduce the two results
      fun.(left_result, right_result)
    end
  end

end


# slow_double = fn(x) -> :timer.sleep(1000); x * 2 end
#
# IO.puts "running sequential"
# {seq_time, _} = :timer.tc(fn() -> Enum.map([1, 2, 3, 4], slow_double) end)
# IO.puts "took #{seq_time / 1000000}s"
#
# IO.puts "running parallel"
# {parallel_time, _} = :timer.tc(fn() -> Parallel.map([1, 2, 3, 4], slow_double) end)
# IO.puts "took #{parallel_time / 1000000}s"


slow_add = fn(a, b) -> :timer.sleep(1000); a + b end

IO.puts "running sequential"
{seq_time, result} = :timer.tc(fn() -> Enum.reduce([1, 2, 3, 4, 5, 6], slow_add) end)
IO.puts "result is #{result}; took #{seq_time / 1000000}s"

IO.puts "running parallel"
{parallel_time, _} = :timer.tc(fn() -> Parallel.reduce([1, 2, 3, 4, 5, 6], slow_add) end)
IO.puts "result is #{result}; took #{parallel_time / 1000000}s"
